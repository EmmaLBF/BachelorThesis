{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import AST (emptyFunctionInfo)
import AbsLang
import C (basicCompile, liftLambdasAndMerge, printCCode)
import CDefs (CStatement)
import Data.Typeable (Typeable)
import DeadCodePass
import DemotePass (demoteClosures, demotePairsPass)
import InlinePass
import System.IO

-- ─────────────────────────────────────────────
--  Optimisation Loop
-- ─────────────────────────────────────────────

keepOptimising :: CStatement -> CStatement
keepOptimising body =
  let body1 = demoteClosures body emptyFunctionInfo
      body2 = inline body1
      body3 = removeEnvs body2
      body4 = propagateCopies body3
      body5 = removeSingleVars body4 body4 emptyFunctionInfo
      body6 = removeUselessStmt body5
  in if body == body6 then body6 else keepOptimising body6

optimiseRun :: CStatement -> CStatement
optimiseRun body = demotePairsPass (keepOptimising body)

-- ─────────────────────────────────────────────
--  Running the Compiler
-- ─────────────────────────────────────────────

-- takes name of file, program to run and three bools:
-- should lambdas be merged, should the main opt loop be run, should pairs be demoted
run :: Typeable a => String -> Lang a -> Bool -> Bool -> IO ()
run progPath progCode canMerge canOpt = do
  let (c, fresh'') = C.basicCompile progCode
  let (cbody, parentParams) = C.liftLambdasAndMerge canMerge c fresh''

  -- optimise
  let cbody' = if canOpt then optimiseRun cbody else cbody
  let finalBody = cleanSkip cbody'
  let content = printCCode finalBody parentParams

  -- writing to file
  let fileName = "outputs/" ++ progPath ++ ".c"
  handle <- openFile fileName WriteMode
  hPutStrLn handle content
  hClose handle
  putStrLn $ "Successfully wrote to " ++ fileName

main :: IO ()
main = do
  let progsInt =
        [ ("gcdLangCall", gcdLangCall),
          ("fibCall", fibCall),
          ("sumListCall", sumListCall),
          ("lenListCall", lenListCall),
          ("ackermannCall", ackermannCall),
          ("fibFastCall", fibFastCall),
          ("collatzCall", collatzCall),
          ("powerCall", powerCall)
        ]
  let progsList = [("mapListCall", mapListCall), ("mergeSortCall", mergeSortCall)]
  let progsQueen = [("nQueensCall1", nQueensCall1), ("nQueensCall", nQueensCall)]

  -- basic compilation
  mapM_ (\(name, prog) -> run ("basic/" ++ name) prog False False) progsInt
  mapM_ (\(name, prog) -> run ("basic/" ++ name) prog False False) progsList
  mapM_ (\(name, prog) -> run ("basic/" ++ name) prog False False) progsQueen

  -- merged lambdas
  mapM_ (\(name, prog) -> run ("mergedLams/" ++ name) prog True False) progsInt
  mapM_ (\(name, prog) -> run ("mergedLams/" ++ name) prog True False) progsList
  mapM_ (\(name, prog) -> run ("mergedLams/" ++ name) prog True False) progsQueen

  -- fully optimised
  mapM_ (\(name, prog) -> run ("optimised/" ++ name) prog True True) progsInt
  mapM_ (\(name, prog) -> run ("optimised/" ++ name) prog True True) progsList
  mapM_ (\(name, prog) -> run ("optimised/" ++ name) prog True True) progsQueen