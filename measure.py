import re
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

def get_stats(path_out, outputs):

    time_result = subprocess.run(
        ["/usr/bin/time", "-v", f"./{path_out}"],
        capture_output=True, text=True
    )
    for line in time_result.stderr.splitlines():
        line = line.strip()
        if any(k in line for k in [
            "Maximum resident",
            "Major (requiring I/O) page faults",
            "Minor (reclaiming a frame) page faults",
            "Voluntary context switches",
            "Involuntary context switches"
        ]):
            splitLine = line.split(':', maxsplit=1)
            nameLine = splitLine[0].replace(' ', '')
            outputs[nameLine] = splitLine[1]

    # Valgrind memcheck: malloc/free counts
    memcheck_result = subprocess.run(
        ["valgrind", f"./{path_out}"],
        capture_output=True, text=True
    )
    for line in memcheck_result.stderr.splitlines():
        if "total heap usage" in line:
            data = line.split("total heap usage:")[1]
            nums = re.findall(r'[\d,]+', data)
            outputs["Allocs"] = nums[0].replace(',', '')
            outputs["Frees"] = nums[2].replace(',', '')
            outputs["BytesAlloced"] = nums[4].replace(',', '')

    # Valgrind callgrind: instruction count
    callgrind_result = subprocess.run(
        ["valgrind", "--tool=callgrind", "--callgrind-out-file=/dev/null",
        f"./{path_out}"],
        capture_output=True, text=True
    )
    for line in callgrind_result.stderr.splitlines():
        if "I   refs" in line:
            outputs["InstructionCount"] = line.split(':')[1].strip().replace(',', '')
            break

def compile_and_run_c(c_file, trial, index_to_remove, new_line):
    try:
        path     = "outputs/" + c_file + ".c"
        path_out = "outputs/" + c_file
        
        print("\n" + ("-" * 30))
        print(f"{BOLD}{RED}Running: {c_file} | {trial} {RESET}")

        # replace call with last line
        if (index_to_remove > 0):
            with open(path) as f:
                lines = f.readlines()
                target_index = len(lines) - index_to_remove
                lines[target_index] = new_line
            with open(path, 'w', encoding='utf-8') as f:
                f.writelines(lines)

        # Compile
        compile_cmd = ["gcc", path, "-o", path_out]
        subprocess.run(compile_cmd, check=True)

        # Run with timing and memory
        start = time.perf_counter()
        result = subprocess.run(
            [f"./{path_out}"] if os.name != "nt" else [path_out],
            check=True
            , stdout=subprocess.DEVNULL
        )
        elapsed = time.perf_counter() - start

        # resource.getrusage tracks the last child process
        usage = resource.getrusage(resource.RUSAGE_CHILDREN)

        outputs = {"ElapsedTime(s)": elapsed, "UserCPUtime(s)": usage.ru_utime, "SysCPUtime(s)": usage.ru_stime, "MaxRSS(kb)": usage.ru_maxrss}
        get_stats(path_out, outputs)
        return outputs

    except subprocess.CalledProcessError as e:
        print(f"Compilation/Execution failed: {e}")
    except Exception as e:
        print(f"Unexpected error: {e}")

def only_run(c_file):
    try:
        path     = "outputs/" + c_file + ".c"
        path_out = "outputs/" + c_file
        
        print(f"{BOLD}{RED}Running: {c_file} {RESET}")

        # Compile
        compile_cmd = ["gcc", path, "-o", path_out]
        subprocess.run(compile_cmd, check=True)

        # Run with timing and memory
        result = subprocess.run(
            [f"./{path_out}"] if os.name != "nt" else [path_out],
            check=True
        )
    except subprocess.CalledProcessError as e:
        print(f"Compilation/Execution failed: {e}")
    except Exception as e:
        print(f"Unexpected error: {e}")

progs = ["fibCall", "gcdLangCall", "sumListCall", "lenListCall", "mapListCall", "mergeSortCall"]
trials = [3, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000]

def runTrials(path_half):
    path_out = "outputs/" + path_half
    path = path_out + ".c"

    # Get Code Stats (only want this once)
    with open(path) as f:
        loc = sum(1 for line in f if line.strip())
    compile_cmd = ["gcc", path, "-o", path_out]
    subprocess.run(compile_cmd, check=True)
    binary_size = os.path.getsize(path_out)
    nm = subprocess.run(["nm", path_out], capture_output=True, text=True)
    symbol_count = len(nm.stdout.strip().splitlines())
    
    print(f"\n------CodeStats")
    print(f"Lines(non-empty): {loc}")
    print(f"Binary_size(bytes): {binary_size}")
    print(f"Symbol_count: {symbol_count}")

    all_outputs = {}
    for trial in trials:
        res = compile_and_run_c(path_half, trial, 4, "  printList(v0(LIST" + str(trial) + "()));\n")
        all_outputs[trial] = res
    
    cols = list(next(iter(all_outputs.values())).keys())

    print("n " + " ".join(cols))
    for n, row in all_outputs.items():
        print(str(n) + " " + " ".join(str(row[c]).strip() for c in cols))



# print("LamMerged MERGESORT ******")
# runTrials("mergedLams/mergeSortCall")

# print("Basic MERGESORT ******")
# runTrials("inlined/mergeSortCall")

# print("Testing ******")
# print("\n" + ("-" * 30))
# for prog in progs:
#     only_run(prog)


print("Test All Basic ******")
for folder in ["baselines", "mergedLams", "removedClosureAllocs", "inlined"]:
    print("\n" + ("-" * 30))
    print("\n" + folder)
    for prog in progs:
        only_run(folder + "/" + prog)