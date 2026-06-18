{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import C ( translateALToC, runLiftAndMerge, printCode )
import CDefs ( CStatement )
import qualified AbsLang as AL

import Data.Typeable ( Typeable )
import System.IO

import AST ( emptyFunctionInfo )
import InlinePass
import DemotePass ( demotePairsPass, demoteClosures )
import DeadCodePass

{-
gcc ./outputs/mergeSortCall_output.c -o ./outputs/mergeSortCall_output
./outputs/mergeSortCall_output
-}

keepOptimising :: CStatement -> CStatement
keepOptimising body =
    let body1 = demoteClosures body emptyFunctionInfo
        body2 = inlinePass body1
        body3 = envRemovalPass body2
        body4 = eliminateAliases body3
        body5 = removeSingleVars body4 body4 emptyFunctionInfo
        body6 = removeUselessStmt body5
    in  if body == body6 then body6 else keepOptimising body6

optimiseRun :: CStatement -> CStatement
optimiseRun body = demotePairsPass (keepOptimising body)

-- takes name of file, program to run and three bools:
-- should lambdas be merged, should the main opt loop be run, should pairs be demoted
run :: Typeable a => String -> AL.Lang a -> Bool -> Bool -> IO ()
run progPath progCode canMerge canOpt = do
    let (c, fresh'') = C.translateALToC progCode
    let (cbody, parentParams) = C.runLiftAndMerge canMerge c fresh''

    -- optimise
    let cbody' = if canOpt then optimiseRun cbody else cbody
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
    mapM_ (\(name, prog) -> run ("basic/" ++ name) prog False False) progsInt
    mapM_ (\(name, prog) -> run ("basic/" ++ name) prog False False) progsList
    mapM_ (\(name, prog) -> run ("basic/" ++ name) prog False False) progsQueen

    -- merged lams
    mapM_ (\(name, prog) -> run ("mergedLams/" ++ name) prog True False) progsInt
    mapM_ (\(name, prog) -> run ("mergedLams/" ++ name) prog True False) progsList
    mapM_ (\(name, prog) -> run ("mergedLams/" ++ name) prog True False) progsQueen

    -- optimised (with pairs)
    mapM_ (\(name, prog) -> run ("optimised/" ++ name) prog True True) progsInt
    mapM_ (\(name, prog) -> run ("optimised/" ++ name) prog True True) progsList
    mapM_ (\(name, prog) -> run ("optimised/" ++ name) prog True True) progsQueen