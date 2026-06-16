{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LambdaMergePass where

import CDefs
import Utils

-- for a function (int), given the amount of params, check that every call site has at least that many applications
checkCallExpr :: Int -> Int -> CExpression -> Bool
checkCallExpr fun params expr@CallExpr{} =
    let (f, args) = CDefs.collectArgs expr
    in case f of
        Var _ i | i == fun -> length args >= params
        _ -> all (\(CArg _ x) -> case x of (Var _ i) | i == fun -> False; _ -> True) args
checkCallExpr fun params e = and [checkCallExpr fun params c | c <- childrenExpr e]

checkCallStmt :: Int -> Int -> CStatement -> Bool
checkCallStmt ifun params s = and ([checkCallStmt ifun params c | c <- childrenStmt s] ++ [checkCallExpr ifun params e | e <- childExprsStmt s])

-- return merged and map of functions to their new number of params
-- the whole program unchanged, the current stmt, map of changed params
mergeLambdas :: CStatement -> CStatement -> CStatement
mergeLambdas prog (DefFun tret ifun params body) =
    case body of
        (Seq (DefFun tret1 ifun1 params1 body1) (Return (Var _ i))) ->
            let newParams = params ++ params1
                canMerge = checkCallStmt ifun (length newParams) prog && ifun1 == i
            in  if canMerge
                then mergeLambdas prog (DefFun tret1 ifun newParams body1)
                else DefFun tret ifun params (mergeLambdas prog body)
        _ -> DefFun tret ifun params (mergeLambdas prog body)
mergeLambdas prog stmt = mapChildrenStmt (mergeLambdas prog) id stmt
