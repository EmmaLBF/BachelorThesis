import subprocess
import os

RED     = "\033[31m"
BOLD    = "\033[1m"
RESET   = "\033[0m"

def run(c_file):
    try:
        path = "outputs/" + c_file + ".c"
        path_out = "outputs/" + c_file
        
        print(f"{BOLD}{RED}Running: {c_file} {RESET}")

        compile_cmd = ["gcc", path, "-o", path_out]
        subprocess.run(compile_cmd, check=True)

        result = subprocess.run(
            [f"./{path_out}"] if os.name != "nt" else [path_out],
            check=True
        )

    except subprocess.CalledProcessError as e:
        print(f"Compilation/Execution failed: {e}")
    except Exception as e:
        print(f"Unexpected error: {e}")

progsTest = [
    ("ackermannCall", "15"),
    ("collatzCall", "3"),
    ("fibCall", "5"),
    ("fibFastCall", "8"),
    ("gcdLangCall", "10"),
    ("lenListCall", "3"),
    ("mapListCall", "2 4 6 "),
    ("mergeSortCall", "3 4 6"),
    ("nQueensCall", "2"),
    ("nQueensCall1", "2"),
    ("powerCall", "16"),
    ("sumListCall", "6") ]

print("Test All Basic ******")
for folder in ["basic", "mergedLams", "optimised"]:
    print("\n" + ("-" * 30))
    print("\n" + folder)
    for (prog, expected) in progsTest:
        run(folder + "/" + prog)
        print(f"Expected: {expected}")