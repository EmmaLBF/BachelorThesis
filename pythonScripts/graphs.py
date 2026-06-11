"""
Generate comparison charts from benchmark_results.csv.

Produces, per program:
  - line plots vs input size n, one line per version, for the key metrics
    (instruction count, allocations, median time) with variance shown on time
  - box plots of the raw timing samples per version at the largest n
And across programs:
  - grouped bar charts for fixed-size metrics (binary size, symbol count)

Deterministic metrics (instruction count, allocs) are the headline; timing is
shown with its distribution so the comparison is honest about noise.
"""

import os
import ast
import pandas as pd
import matplotlib.pyplot as plt

CSV = "pythonScripts/benchmarks/20260610_180423.csv"
OUTDIR = "./pythonScripts/charts"
os.makedirs(OUTDIR, exist_ok=True)

# Stable colour per version folder so the legend is learned once.
VERSION_COLOURS = {}
_PALETTE = plt.rcParams["axes.prop_cycle"].by_key()["color"]


def colour_for(version):
    if version not in VERSION_COLOURS:
        VERSION_COLOURS[version] = _PALETTE[len(VERSION_COLOURS) % len(_PALETTE)]
    return VERSION_COLOURS[version]


def short_version(folder):
    if folder == "outputs/optimised/":
        return "Optimised"
    elif folder == "outputs/mergedLams/":
        return "Merged Lambdas"
    elif folder == "outputs/baselines/":
        return "Basic"
    elif folder == "cProgs/":
        return "Baseline"
    return "nothing"


def parse_samples(cell):
    if not isinstance(cell, str) or not cell:
        return []
    return [float(x) for x in cell.split(";") if x]


# ---------------------------------------------------------------------------
# Line plots vs n  (one figure per program per metric)
# ---------------------------------------------------------------------------

def line_plot(df, program, metric, ylabel, logy=False, errcol=None):
    sub = df[df["program"] == program]
    if sub.empty or metric not in sub.columns:
        return
    fig, ax = plt.subplots(figsize=(7, 4.5))
    for folder, g in sub.groupby("folder"):
        g = g.sort_values("n")
        ver = short_version(folder)
        c = colour_for(ver)
        if errcol and errcol in g.columns:
            ax.errorbar(g["n"], g[metric], yerr=g[errcol], label=ver,
                        color=c, marker="o", capsize=3)
        else:
            ax.plot(g["n"], g[metric], label=ver, color=c, marker="o")
    ax.set_xlabel("input size n")
    ax.set_ylabel(ylabel)
    if logy:
        ax.set_yscale("log")
    ax.set_title(f"{program}: {ylabel} vs n")
    ax.legend(title="version")
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    path = os.path.join(OUTDIR, f"{program}_{metric.replace('(', '').replace(')', '').replace('/', '_')}.png")
    fig.savefig(path, dpi=150)
    plt.close(fig)
    print("wrote", path)


# ---------------------------------------------------------------------------
# Box plots of raw timing samples at the largest n  (one figure per program)
# ---------------------------------------------------------------------------

def timing_boxplot(df, program):
    sub = df[df["program"] == program]
    if sub.empty or "Time_samples(s)" not in sub.columns:
        return
    n_max = sub["n"].max()
    sub = sub[sub["n"] == n_max]
    data, labels = [], []
    for folder, g in sub.groupby("folder"):
        samples = parse_samples(g.iloc[0]["Time_samples(s)"])
        if samples:
            data.append(samples)
            labels.append(short_version(folder))
    if not data:
        return
    fig, ax = plt.subplots(figsize=(7, 4.5))
    bp = ax.boxplot(data, tick_labels=labels, patch_artist=True, showmeans=True)
    for patch, lab in zip(bp["boxes"], labels):
        patch.set_facecolor(colour_for(lab))
        patch.set_alpha(0.6)
    ax.set_ylabel("wall-clock time (s)")
    ax.set_title(f"{program}: timing distribution at n={n_max}")
    ax.grid(True, axis="y", alpha=0.3)
    fig.tight_layout()
    path = os.path.join(OUTDIR, f"{program}_timing_box.png")
    fig.savefig(path, dpi=150)
    plt.close(fig)
    print("wrote", path)


# ---------------------------------------------------------------------------
# Grouped bar chart for a fixed-size metric across programs
# ---------------------------------------------------------------------------

def grouped_bar(df, metric, ylabel):
    if metric not in df.columns:
        return
    # one value per (program, version): take the row at the largest n
    rows = []
    for (program, folder), g in df.groupby(["program", "folder"]):
        g = g.sort_values("n")
        rows.append((program, short_version(folder), g.iloc[-1][metric]))
    if not rows:
        return
    pivot = pd.DataFrame(rows, columns=["program", "version", "value"])
    programs = sorted(pivot["program"].unique())
    versions = list(dict.fromkeys(pivot["version"]))
    width = 0.8 / max(len(versions), 1)

    fig, ax = plt.subplots(figsize=(max(7, 1.6 * len(programs)), 4.5))
    for i, ver in enumerate(versions):
        vals = [pivot[(pivot.program == p) & (pivot.version == ver)]["value"].mean()
                for p in programs]
        xs = [j + i * width for j in range(len(programs))]
        ax.bar(xs, vals, width, label=ver, color=colour_for(ver), alpha=0.8)
    ax.set_xticks([j + width * (len(versions) - 1) / 2 for j in range(len(programs))])
    ax.set_xticklabels(programs, rotation=30, ha="right")
    ax.set_ylabel(ylabel)
    ax.set_title(f"{ylabel} by program and version")
    ax.legend(title="version")
    ax.grid(True, axis="y", alpha=0.3)
    fig.tight_layout()
    path = os.path.join(OUTDIR, f"bar_{metric.replace('(', '').replace(')', '').replace('/', '_')}.png")
    fig.savefig(path, dpi=150)
    plt.close(fig)
    print("wrote", path)


def main():
    df = pd.read_csv(CSV)
    # numeric coercion for safety
    for col in ["n", "InstructionCount", "Allocs", "Frees", "BytesAlloced",
                "Time_median(s)", "Time_stdev(s)", "BinarySize(bytes)", "SymbolCount"]:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce")

    programs = df["program"].unique()
    for p in programs:
        # Deterministic headline metrics (clean lines)
        line_plot(df, p, "InstructionCount", "instruction count", logy=True)
        line_plot(df, p, "Allocs", "allocations", logy=True)
        # Timing with variance shown
        line_plot(df, p, "Time_median(s)", "median time (s)",
                  logy=True, errcol="Time_stdev(s)")
        # Distribution of raw timing samples
        timing_boxplot(df, p)

    # Fixed-size comparisons across programs
    grouped_bar(df, "BinarySize(bytes)", "binary size (bytes)")
    grouped_bar(df, "SymbolCount", "symbol count")


if __name__ == "__main__":
    main()