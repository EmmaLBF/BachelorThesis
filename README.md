# Compiler for an embedded functional language to C

This repository contains the files and materials for my bachelor thesis project "Compiling a functional language to C: Implementation in Haskell". The structure below gives a general overview of how the repository is organized.

## Repository Structure

- `baselines/` — baseline C implementations of the benchmark programs.
- `benchmarking/` — scripts for benchmarking, collected data and graphs.
    - `experiment.py` — script for running benchmarks.
    - `graphs.py` — script for creating graphs from benchmarks.
    - `stats.py` — script for calculating average percentage differences between versions.
    - `test.py` — script for running all programs, printing their output and the expected output.
- `outputs/` — contains the compiled code for each test program at three different optimisation levels: basic, merged lambdas, full optimisation. Also contains `lib.c` which holds the library functions for creating lists and boxing.

## Main Compiler Components

- `AbsLang.hs`: Add a short description here.
- `NamedLang.hs`: Add a short description here.
- `CLang.hs`: Add a short description here.
- `CDefs.hs`: 
- `C.hs`: 

## Main Optimisation Components

- `LambdaMergePass.hs`: 
- `DeadCodePass.hs`: 
- `DemotePass.hs`: 
- `InlinePass.hs`: 

## How to Run

Build the cabal file, load `Main.hs` in ghci and then run `main` to compile all of the example programs.

## Notes

- Add any important context here.
- Add any assumptions or limitations here.
- Add references or external resources here.
