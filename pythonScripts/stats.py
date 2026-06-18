import pandas as pd
from scipy.stats import ttest_ind, mannwhitneyu, shapiro
import scipy.stats as stats
import matplotlib.pyplot as plt



def samples(program, folder, n, df):
    row = df[(df.program == program) & (df.folder == folder) & (df.n == n)].iloc[0]
    return [float(x) for x in row["Time_samples(s)"].split(";") if x]

def print_stats(code_name, size, df):
    print(f"TESTING: {code_name}")

    c_code = samples(code_name, "baselines/", size, df)
    opt_code = samples(code_name, "outputs/optimised/", size, df)

    # Null hypothesis is "the data is normal." 
    # So p > 0.05 means you can't reject normality (consistent with normal); p < 0.05 means it's significantly non-normal. 
    # For wall-clock timings you'll often get p < 0.05 because of right-skew from occasional slow runs.
    # The Shapiro test is per-group
    wc, pc = shapiro(c_code)
    wopt, popt = shapiro(opt_code)
    print(f"\nShapiro-Wilk\n    W_c={wc:.4f}, p_c={pc:.4g}\n    W_opt={wopt:.4f}, p_opt={popt:.4g}")
    print("c normal" if pc > 0.05 else "c NOT normal (reject normality)")
    print("opt normal" if popt > 0.05 else "opt NOT normal (reject normality)")

    ttest_res = ttest_ind(c_code, opt_code, equal_var=False) # Welch's t-test
    print("\nT-Test Result")
    print(f"    pvalue = {ttest_res.pvalue}")
    print(f"    df = {ttest_res.df}")
    print(f"    statistic = {ttest_res.statistic}")   

    mannwhitneyu_res = mannwhitneyu(c_code, opt_code, alternative="two-sided")

    print("\nMann-Whitney U-Test")
    print(f"    pvalue = {mannwhitneyu_res.pvalue}")
    print(f"    statistic = {mannwhitneyu_res.statistic}")  


df_merge = pd.read_csv("pythonScripts/benchmarks/mergeAgain_20260617_171111.csv")
print_stats("mergeSortCall", 30000, df_merge)

df_queen1 = pd.read_csv("pythonScripts/benchmarks/Queens1_20260617_162053.csv")
df_queen2 = pd.read_csv("pythonScripts/benchmarks/Queens2_20260617_163557.csv")
df_queen =  pd.concat([df_queen1, df_queen2], ignore_index=True)
print_stats("nQueensCall", 10, df_queen)

c_code_merge = samples("mergeSortCall", "baselines/", 30000, df_merge)
opt_code_merge = samples("mergeSortCall", "outputs/optimised/", 30000, df_merge)

c_code_queen = samples("nQueensCall", "baselines/", 10, df_queen)
opt_code_queen = samples("nQueensCall", "outputs/optimised/", 10, df_queen)

# q plot to check normality
fig, ax = plt.subplots(4, 1, figsize=(6, 8))
stats.probplot(c_code_merge, dist="norm", plot=ax[0])
ax[0].set_title(f"Q-Q plot: c mergeSort")
stats.probplot(opt_code_merge, dist="norm", plot=ax[1])
ax[1].set_title(f"Q-Q plot: optimised mergeSort")

stats.probplot(c_code_queen, dist="norm", plot=ax[2])
ax[2].set_title(f"Q-Q plot: c nQueens")
stats.probplot(opt_code_queen, dist="norm", plot=ax[3])
ax[3].set_title(f"Q-Q plot: optimised nQueens")

plt.tight_layout()
plt.savefig(f"pythonScripts/charts/qplot.png", dpi=150)
plt.close(fig)



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