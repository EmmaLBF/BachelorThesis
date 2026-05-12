import subprocess
import os
import time

import subprocess
import os
import time
import resource

def get_stats(path_out):

    # Binary size
    binary_size = os.path.getsize(path_out)

    # Symbol count
    nm = subprocess.run(["nm", path_out], capture_output=True, text=True)
    symbol_count = len(nm.stdout.strip().splitlines())

    # Perf: instructions, cycles, cache misses, branch misses
    perf = subprocess.run(
        ["perf", "stat", "-e",
         "instructions,cycles,cache-misses,branch-misses",
         f"./{path_out}"],
        capture_output=True, text=True
    )
    # perf writes to stderr
    print(perf.stderr)

    # Valgrind malloc count
    valgrind = subprocess.run(
        ["valgrind", "--tool=massif", f"./{path_out}"],
        capture_output=True, text=True
    )

    print(f"Binary size:   {binary_size} bytes")
    print(f"Symbol count:  {symbol_count}")

def compile_and_run_c(c_file):
    try:
        path     = "outputs/baselines/" + c_file + ".c"
        path_out = "outputs/baselines/" + c_file

        # Lines of code
        with open(path) as f:
            loc = sum(1 for line in f if line.strip())

        # Compile
        compile_cmd = ["gcc", path, "-o", path_out]
        print(f"Compiling: {' '.join(compile_cmd)}")
        subprocess.run(compile_cmd, check=True)

        # Run with timing and memory
        print(f"Running: {c_file}")
        start = time.perf_counter()
        result = subprocess.run(
            [f"./{path_out}"] if os.name != "nt" else [path_out],
            check=True
        )
        elapsed = time.perf_counter() - start

        # resource.getrusage tracks the last child process
        usage = resource.getrusage(resource.RUSAGE_CHILDREN)

        print(f"Elapsed time:  {elapsed:.4f}s")
        print(f"Max RSS:       {usage.ru_maxrss / 1024:.2f} MB")   # bytes on Linux
        print(f"User CPU time: {usage.ru_utime:.4f}s")
        print(f"Sys CPU time:  {usage.ru_stime:.4f}s")
        print(f"Lines of code (non-empty): {loc}")
        get_stats(path_out)

    except subprocess.CalledProcessError as e:
        print(f"Compilation/Execution failed: {e}")
    except Exception as e:
        print(f"Unexpected error: {e}")

compile_and_run_c("mergeSort/100")