import os
import re
import csv
import time
import shutil
import subprocess
import statistics

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

# Number of times each binary is re-run to measure (noisy) wall-clock timing.
# Deterministic metrics (instruction count, allocs) are measured once.
TIMING_REPS = 30
# How many leading timing runs to discard as warm-up (cache / page-in effects).
WARMUP_REPS = 3

# Optimisation level for gcc. Be deliberate: -O0 keeps the generated C close to
# what your compiler emitted (best for comparing YOUR passes); -O2 shows what
# survives a production C compiler. Pick one and report it.
GCC_OPT = "-O0"

# Pin to a single core to cut scheduler noise. Set to None to disable.
PIN_CORE = 0

# ANSI colours (optional, cosmetic)
BOLD = "\033[1m"
RED = "\033[31m"
RESET = "\033[0m"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _run_prefix():
    """Command prefix to pin the process to one core, if requested + available."""
    if PIN_CORE is not None and os.name != "nt" and shutil.which("taskset"):
        return ["taskset", "-c", str(PIN_CORE)]
    return []


def _have(tool):
    return shutil.which(tool) is not None


def compile_c(path, path_out):
    """Compile a single C file at the configured optimisation level."""
    compile_cmd = ["gcc", GCC_OPT, path, "-o", path_out]
    subprocess.run(compile_cmd, check=True)


def patch_last_lines(path, index_to_remove, new_line):
    """Replace a line near the bottom of the file (used to set input size n)."""
    if index_to_remove <= 0:
        return
    with open(path) as f:
        lines = f.readlines()
    target_index = len(lines) - index_to_remove
    lines[target_index] = new_line
    with open(path, "w", encoding="utf-8") as f:
        f.writelines(lines)


# ---------------------------------------------------------------------------
# Deterministic metrics  (measured ONCE -- repeatable, machine-independent)
# ---------------------------------------------------------------------------

def deterministic_stats(path_out):
    """Instruction count (callgrind) and heap alloc/free/bytes (valgrind).

    These are deterministic: re-running gives (essentially) identical numbers,
    so a single measurement is enough. They are the headline metrics for
    comparing compiler-optimisation versions.
    """
    out = {}
    run_target = _run_prefix() + [f"./{path_out}"]

    # --- valgrind memcheck: malloc / free / bytes -------------------------
    if _have("valgrind"):
        memcheck = subprocess.run(
            ["valgrind", "--error-exitcode=0"] + [f"./{path_out}"],
            capture_output=True, text=True
        )
        for line in memcheck.stderr.splitlines():
            if "total heap usage" in line:
                data = line.split("total heap usage:")[1]
                nums = re.findall(r"[\d,]+", data)
                # format: "N allocs, M frees, B bytes allocated"
                out["Allocs"] = nums[0].replace(",", "")
                out["Frees"] = nums[1].replace(",", "")
                out["BytesAlloced"] = nums[2].replace(",", "")

        # --- valgrind callgrind: instruction count ------------------------
        callgrind = subprocess.run(
            ["valgrind", "--tool=callgrind",
             "--callgrind-out-file=/dev/null"] + [f"./{path_out}"],
            capture_output=True, text=True
        )
        for line in callgrind.stderr.splitlines():
            # line looks like:  ==12345== I   refs:      1,234,567
            if "refs:" in line and " I " in line:
                out["InstructionCount"] = (
                    line.split("refs:")[1].strip().replace(",", "")
                )
                break
    else:
        print(f"{RED}valgrind not found: skipping instruction/alloc counts{RESET}")

    return out


# ---------------------------------------------------------------------------
# Timing metrics  (measured MANY TIMES -- noisy, need statistics)
# ---------------------------------------------------------------------------

def timing_stats(path_out, reps=TIMING_REPS, warmup=WARMUP_REPS):
    """Run the binary `reps` times, discard `warmup`, and summarise wall-clock.

    Reports median (robust), minimum (least-disturbed run) and stdev. Timing is
    done in its OWN loop, never alongside valgrind, and pinned to one core.
    Peak RSS is taken from /usr/bin/time -v on a single representative run.
    """
    times = []
    run_target = _run_prefix() + [f"./{path_out}"]
    for i in range(reps):
        start = time.perf_counter()
        subprocess.run(run_target, check=True, stdout=subprocess.DEVNULL)
        elapsed = time.perf_counter() - start
        if i >= warmup:
            times.append(elapsed)

    out = {
        "Time_median(s)": statistics.median(times),
        "Time_min(s)": min(times),
        "Time_max(s)": max(times),
        "Time_stdev(s)": statistics.stdev(times) if len(times) > 1 else 0.0,
        "Time_samples(s)": ";".join(f"{t:.9f}" for t in times)
    }

    # Peak resident memory from one /usr/bin/time -v run (deterministic-ish).
    if os.name != "nt" and os.path.exists("/usr/bin/time"):
        tr = subprocess.run(
            ["/usr/bin/time", "-v"] + run_target,
            capture_output=True, text=True
        )
        for line in tr.stderr.splitlines():
            line = line.strip()
            if "Maximum resident set size" in line:
                out["MaxRSS(kb)"] = line.split(":", 1)[1].strip()
    return out


# ---------------------------------------------------------------------------
# Code-size metrics  (measured ONCE per version, independent of input size)
# ---------------------------------------------------------------------------

def code_stats(path, path_out):
    with open(path) as f:
        loc = sum(1 for line in f if line.strip())
    compile_c(path, path_out)
    binary_size = os.path.getsize(path_out)
    symbol_count = 0
    if _have("nm"):
        nm = subprocess.run(["nm", path_out], capture_output=True, text=True)
        symbol_count = len(nm.stdout.strip().splitlines())
    return {
        "Lines(non-empty)": loc,
        "BinarySize(bytes)": binary_size,
        "SymbolCount": symbol_count,
    }


# ---------------------------------------------------------------------------
# Orchestration
# ---------------------------------------------------------------------------

def measure_one(folder, c_file, distance_from_bottom, new_line):
    """Full measurement for one (program, input-size) point.

    Compiles once, takes deterministic metrics once, then runs the timing loop
    separately. Returns a merged dict of metrics.
    """
    path = folder + c_file + ".c"
    path_out = folder + c_file

    patch_last_lines(path, distance_from_bottom, new_line)
    compile_c(path, path_out)

    result = {}
    # Deterministic first (under valgrind) ...
    result.update(deterministic_stats(path_out))
    # ... then timing in its own clean loop (no valgrind running).
    result.update(timing_stats(path_out))
    return result


def run_trials(path_half, folder, distance_from_bottom,
               new_line_first, new_line_second, sizes, csv_writer=None):
    """Sweep input sizes for one program/version and print a results table.

    If `csv_writer` is given, one row per (program, folder, size) is appended,
    including the code-size metrics so every row is self-describing.
    """
    path_out = folder + path_half
    path = path_out + ".c"

    cs = code_stats(path, path_out)
    print(f"\n------ CodeStats: {path_half} [{folder}] (gcc {GCC_OPT}) ------")
    for k, v in cs.items():
        print(f"{k}: {v}")

    all_outputs = {}
    for n in sizes:
        new_line = new_line_first + str(n) + new_line_second
        print(f"{BOLD}{RED}Measuring {path_half} [{folder}] | n={n}{RESET}")
        try:
            all_outputs[n] = measure_one(
                folder, path_half, distance_from_bottom, new_line
            )
        except subprocess.CalledProcessError as e:
            print(f"Compilation/Execution failed at n={n}: {e}")
        except Exception as e:
            print(f"Unexpected error at n={n}: {e}")

    if not all_outputs:
        print("No successful measurements.")
        return all_outputs

    cols = list(next(iter(all_outputs.values())).keys())
    print("\nn " + " ".join(cols))
    for n, row in all_outputs.items():
        print(str(n) + " " + " ".join(str(row.get(c, "")).strip() for c in cols))
        if csv_writer is not None:
            record = {
                "program": path_half,
                "folder": folder,
                "n": n,
                **cs,
                **row,
            }
            csv_writer.writerow(record)

    return all_outputs


def only_run(c_file, folder="outputs/"):
    """Just compile and run once (no measurement)."""
    path = folder + c_file + ".c"
    path_out = folder + c_file
    print(f"{BOLD}{RED}Running: {c_file}{RESET}")
    try:
        compile_c(path, path_out)
        subprocess.run(_run_prefix() + [f"./{path_out}"], check=True)
    except subprocess.CalledProcessError as e:
        print(f"Compilation/Execution failed: {e}")


# ---------------------------------------------------------------------------
# Per-program configuration. Each logical program may exist in SEVERAL folders
# (one per compiler version compared). If a version needs a different patch
# rule (e.g. the entry point differs), give it its own entry.
#
# For a given program and size n, the patched line becomes:
#     prefix + str(n) + suffix
# replacing the line at  len(lines) - distance_from_bottom.
# ---------------------------------------------------------------------------

MERGESORT = [
    {
        "name": "mergeSortCall",
        "folders": ["outputs/baselines/", "outputs/mergedLams/", "outputs/optimised/", "cProgs/"],
        "sizes": [100, 1000, 5000, 10000, 15000, 20000, 22000, 25000, 28000, 30000],
        "distance_from_bottom": 4,
        "prefix": "  printListInt(v0(LIST(",
        "suffix": ", 42)));\n",
    }
]

QUEENS1 = [
    {
        "name": "nQueensCall",
        "folders": ["outputs/baselines/", "outputs/mergedLams/"],
        "sizes": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        "distance_from_bottom": 4,
        "prefix": "  printInt(v0(v106(",
        "suffix": ")));\n",
    }
]

QUEENS2 = [
    {
        "name": "nQueensCall",
        "folders": ["outputs/optimised/", "cProgs/"],
        "sizes": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        "distance_from_bottom": 7,
        "prefix": "  int v5 = ",
        "suffix": ";\n",
    }
]

OUTPUT_CSV = f"pythonScripts/benchmarks/mergeAgain_{time.strftime('%Y%m%d_%H%M%S')}.csv"

if __name__ == "__main__":
    rows = []
    # A tiny adapter so run_trials can "write" rows into our list via .writerow.
    collector = type("RowCollector", (), {"writerow": staticmethod(rows.append)})()

    for prog in MERGESORT:
        for folder in prog["folders"]:
            run_trials(
                path_half=prog["name"],
                folder=folder,
                distance_from_bottom=prog["distance_from_bottom"],
                new_line_first=prog["prefix"],
                new_line_second=prog["suffix"],
                sizes=prog["sizes"],
                csv_writer=collector,
            )

    # Write everything with a unified header (union of keys, first-seen order).
    if rows:
        header = []
        for r in rows:
            for k in r:
                if k not in header:
                    header.append(k)
        with open(OUTPUT_CSV, "w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=header)
            writer.writeheader()
            writer.writerows(rows)
        print(f"\n{BOLD}Wrote {len(rows)} rows to {OUTPUT_CSV}{RESET}")
    else:
        print("No results to write.")