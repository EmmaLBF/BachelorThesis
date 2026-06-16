{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import C ( translateALToC, runLiftAndMerge, printCode )
import CDefs ( CStatement )
import qualified AbsLang as AL

import Data.Typeable ( Typeable )
import System.IO

import Utils ( getFunsWithParams )
import AST ( emptyFunctionInfo )
import InlinePass ( inlineUntilFixed )
import DemotePairsPass ( canBeByValue, demotePairs )
import DeadCodePass
import Control.Monad.State ( evalState )

{-
gcc ./outputs/mergeSortCall_output.c -o ./outputs/mergeSortCall_output
./outputs/mergeSortCall_output
-}

keepOptimising :: CStatement -> CStatement
keepOptimising body =
    let removeClosuresBody = evalState (removeClosureAllocs body emptyFunctionInfo) [] -- remove closures
        inlinedBody = inlineUntilFixed removeClosuresBody -- inline
        removedEnvParamBody = envParamRemovalPass inlinedBody -- remove env params
        elminatedEnvsAliasesBody = eliminateAliases removedEnvParamBody -- remove aliases and local envs
        removedVarsBody = removeSingleVars elminatedEnvsAliasesBody elminatedEnvsAliasesBody emptyFunctionInfo -- remove vars that are used <= 1 times
        removedUselessBody = removeUselessStmt removedVarsBody -- remove useless logic and casts
    in  if body == removedUselessBody
        then removedUselessBody
        else keepOptimising removedUselessBody

optimiseRun :: CStatement -> Bool -> CStatement
optimiseRun body canPair =
    let optimisedBody = keepOptimising body
    in if canPair
        then demotePairs optimisedBody (canBeByValue optimisedBody) (getFunsWithParams optimisedBody)
        else optimisedBody

-- takes name of file, program to run and three bools:
-- should lambdas be merged, should the main opt loop be run, should pairs be demoted
run :: Typeable a => String -> AL.Lang a -> Bool -> Bool -> Bool -> IO ()
run progPath progCode canMerge canOpt canPair = do
    let (c, fresh'') = C.translateALToC progCode
    let (cbody, parentParams) = C.runLiftAndMerge canMerge c fresh''

    -- optimise
    let cbody' = if canOpt then optimiseRun cbody canPair else cbody
    let finalBody = cleanSkip cbody'
    let content = printCode finalBody parentParams
    
    -- writing to file
    let fileName = "outputs/" ++ progPath ++ ".c"
    handle <- openFile fileName WriteMode
    hPutStrLn handle content
    hClose handle
    putStrLn $ "Successfully wrote to " ++ fileName

main :: IO ()
main = do
    let progsInt = [("gcdLangCall", AL.gcdLangCall), ("fibCall", AL.fibCall), ("sumListCall", AL.sumListCall), ("lenListCall", AL.lenListCall)]
    let progsList = [("mapListCall", AL.mapListCall), ("mergeSortCall", AL.mergeSortCall)]
    let progsQueen = [("nQueensCall1", AL.nQueensCall1), ("nQueensCall", AL.nQueensCall)]

    -- basic
    mapM_ (\(name, prog) -> run ("baselines/" ++ name) prog False False False) progsInt
    mapM_ (\(name, prog) -> run ("baselines/" ++ name) prog False False False) progsList
    mapM_ (\(name, prog) -> run ("baselines/" ++ name) prog False False False) progsQueen

    -- merged lams
    mapM_ (\(name, prog) -> run ("mergedLams/" ++ name) prog True False False) progsInt
    mapM_ (\(name, prog) -> run ("mergedLams/" ++ name) prog True False False) progsList
    mapM_ (\(name, prog) -> run ("mergedLams/" ++ name) prog True False False) progsQueen

    -- optimised (with pairs)
    mapM_ (\(name, prog) -> run ("optimised/" ++ name) prog True True True) progsInt
    mapM_ (\(name, prog) -> run ("optimised/" ++ name) prog True True True) progsList
    mapM_ (\(name, prog) -> run ("optimised/" ++ name) prog True True True) progsQueen