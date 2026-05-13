import subprocess
import os
import time

import subprocess
import os
import time
import resource

# colours
RED     = "\033[31m"
GREEN   = "\033[32m"
YELLOW  = "\033[33m"
BLUE    = "\033[34m"
MAGENTA = "\033[35m"
CYAN    = "\033[36m"

# styles
BOLD    = "\033[1m"
RESET   = "\033[0m"

def fmt(label, value, colour=CYAN):
    return f"{BOLD}{colour}{label}{RESET} {value}"

def get_stats(path_out):

    # Binary size
    binary_size = os.path.getsize(path_out)
    print(f"Binary size:   {binary_size} bytes")

    # Symbol count
    nm = subprocess.run(["nm", path_out], capture_output=True, text=True)
    symbol_count = len(nm.stdout.strip().splitlines())
    print(f"Symbol count:  {symbol_count}")

    # Wall time + peak RSS via /usr/bin/time
    print("\n-- Valgrind summary --")
    time_result = subprocess.run(
        ["/usr/bin/time", "-v", f"./{path_out}"],
        capture_output=True, text=True
    )
    for line in time_result.stderr.splitlines():
        line = line.strip()
        if any(k in line for k in [
            "Elapsed (wall clock)", "Maximum resident", 
            "User time", "System time",
            "Major (requiring I/O) page faults",
            "Minor (reclaiming a frame) page faults",
            "Voluntary context switches",
            "Involuntary context switches"
        ]):
            print(line)

    # Valgrind memcheck: malloc/free counts
    print("\n-- Valgrind heap summary --")
    memcheck_result = subprocess.run(
        ["valgrind", f"./{path_out}"],
        capture_output=True, text=True
    )
    for line in memcheck_result.stderr.splitlines():
        if "total heap usage" in line:
            print(line.strip())

    # Valgrind callgrind: instruction count
    callgrind_result = subprocess.run(
        ["valgrind", "--tool=callgrind", "--callgrind-out-file=/dev/null",
        f"./{path_out}"],
        capture_output=True, text=True
    )
    for line in callgrind_result.stderr.splitlines():
        if "I   refs" in line:
            print(f"Instruction count: {line.split(':')[1].strip()}")
            break

def compile_and_run_c(c_file):
    try:
        path     = "outputs/" + c_file + ".c"
        path_out = "outputs/" + c_file

        # Lines of code
        with open(path) as f:
            loc = sum(1 for line in f if line.strip())

        # Compile
        compile_cmd = ["gcc", path, "-o", path_out]
        subprocess.run(compile_cmd, check=True)

        # Run with timing and memory
        print(f"Running: {c_file}")
        start = time.perf_counter()
        result = subprocess.run(
            [f"./{path_out}"] if os.name != "nt" else [path_out],
            check=True,
            stdout=subprocess.DEVNULL
        )
        elapsed = time.perf_counter() - start

        # resource.getrusage tracks the last child process
        usage = resource.getrusage(resource.RUSAGE_CHILDREN)

        print(f"\n------ Run Stats")
        print(f"Elapsed time:  {elapsed:.4f}s")
        print(f"Max RSS:       {usage.ru_maxrss / 1024:.2f} MB")   # bytes on Linux
        print(f"User CPU time: {usage.ru_utime:.4f}s")
        print(f"Sys CPU time:  {usage.ru_stime:.4f}s")

        print(f"\n------ Code Stats")
        print(f"Lines of code (non-empty): {loc}")
        get_stats(path_out)

    except subprocess.CalledProcessError as e:
        print(f"Compilation/Execution failed: {e}")
    except Exception as e:
        print(f"Unexpected error: {e}")

progs = ["fibCall", "gcdLangCall", "sumListCall", "lenListCall", "mapListCall", "mergeSortCall"]
trials = [3, 100, 200, 300, 400, 500, 600, 700, 800, 900]

# print("BASELINES ******")
# for prog in progs:
#     compile_and_run_c("baselines/" + prog)

print("BASELINES MERGESORT ******")
mergeSortPath = "outputs/baselines/mergeSortCall.c"
for trial in trials:
    print("\n" + ("-" * 30))
    print(f"{BOLD}{RED}Running: mergeSort | {trial} {RESET}")
    with open(mergeSortPath) as f:
        lines = f.readlines()
        target_index = len(lines) - 4
        new_line = "  printList(v0(LIST" + str(trial) + "()));\n"
        lines[target_index] = new_line
    with open(mergeSortPath, 'w', encoding='utf-8') as f:
        f.writelines(lines)

    compile_and_run_c("baselines/mergeSortCall")



# print("MERGED ******")
# for prog in progs:
#     compile_and_run_c("merged/" + prog)




# compile_and_run_c("baselines/mergeSort/100")