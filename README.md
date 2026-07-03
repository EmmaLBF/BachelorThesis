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

- `AbsLang.hs`: The embedded functional language + and evaluator to run programs in the language. Holds all the test input programs.
- `NamedLang.hs`: The first-order language, translation to name lambdas with globally unique integers. Does some beta reduction to get rid of higher-order function arguments.
- `CLang.hs`: The imperative IR level, splits NamedLang into statements and expressions. Adds mutable variables, conditional branching and return statements. Has a pass to remove excess copies.
- `CDefs.hs`: The final C IR, has functions for printing actual C code. Also holds some helper functions and data structures.
- `C.hs`: Lambda lifting pass to create environments and closures.s

## Main Optimisation Components

- `LambdaMergePass.hs`: Called on CLang IR, merges lambdas which only exist to nest inner lambdas and are always called with sufficient parameters.
- `DeadCodePass.hs`: Removes dead code and propagates copies.
- `DemotePass.hs`: Demotes objects (environments, closures, pairs) which are allocated on the heap, but never escape the current scope, to the stack.
- `InlinePass.hs`: Inlines functions which are called exactly once.

## Helper Components

- `AST.hs`: Information collecting traversals over the C IR AST
- `Util.hs`: Helper functions

## How to Run

This project was made with GHC version 8.10.7

1. Build the cabal file with `cabal build --ghc-options="-g -fbreak-on-exception"`.
2. Load `Main.hs` in ghci.
3. Run `main` to compile all of the example programs.

If you don't want it to append printing you can get rid of it in C.hs
