import pandas as pd

def samples(program, folder, n, df):
    row = df[(df.program == program) & (df.folder == folder) & (df.n == n)].iloc[0]
    return [float(x) for x in row["Time_samples(s)"].split(";") if x]

df_merge = pd.read_csv("benchmarking/benchmarks/mergeFinal.csv")
df_queen1 = pd.read_csv("benchmarking/benchmarks/queensFinal1.csv")
df_queen2 = pd.read_csv("benchmarking/benchmarks/queensFinal2.csv")
df_queen =  pd.concat([df_queen1, df_queen2], ignore_index=True)

# print diff between merged and basic
def diff(code_name, sizes, df, prog1, prog2):
    all_diffs = []
    for size in sizes:
        basic_code = samples(code_name, prog1, size, df)
        merged_code = samples(code_name, prog2, size, df)
        all_diffs.extend([a - b for a, b in zip(basic_code, merged_code)])
    
    overall_avg = sum(all_diffs) / len(all_diffs)
    print(f"Overall average difference: {overall_avg:.1%}")

mergeSortSizes = [100, 1000, 5000, 10000, 15000, 20000, 22000, 25000, 28000, 30000]
nQueensSizes = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

print("DIFF BASIC -> MERGE")
diff("mergeSortCall", mergeSortSizes, df_merge, "outputs/basic/", "outputs/mergedLams/")
diff("nQueensCall", nQueensSizes, df_queen, "outputs/basic/", "outputs/mergedLams/")

print("DIFF BASIC -> OPT")
diff("mergeSortCall", mergeSortSizes, df_merge, "outputs/basic/", "outputs/optimised/")
diff("nQueensCall", nQueensSizes, df_queen, "outputs/basic/", "outputs/optimised/")

print("DIFF OPT -> BASELINE")
diff("mergeSortCall", mergeSortSizes, df_merge, "outputs/optimised/", "baselines/")
diff("nQueensCall", nQueensSizes, df_queen, "outputs/optimised/", "baselines/")