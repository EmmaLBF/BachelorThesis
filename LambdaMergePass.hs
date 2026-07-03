{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LambdaMergePass where

import CDefs
import Utils

mergeLambdas :: CStatement -> CStatement -> CStatement
mergeLambdas prog (DefFun tret ifun params body) =
  case body of
    (Seq (DefFun tret1 ifun1 params1 body1) (Return (Var _ i)))
      | checkCallStmt ifun (length (params ++ params1)) prog && ifun1 == i ->
        mergeLambdas prog (DefFun tret1 ifun (params ++ params1) body1)
    _ -> DefFun tret ifun params (mergeLambdas prog body)
mergeLambdas prog stmt = mapChildrenStmt (mergeLambdas prog) id stmt

checkCallStmt :: Int -> Int -> CStatement -> Bool
checkCallStmt ifun params s = and ([checkCallStmt ifun params c | c <- childrenStmt s] ++ [checkCallExpr ifun params e | e <- childExprsStmt s])

-- For a function id, given the amount of params, check that every call site has at least that many arguments
checkCallExpr :: Int -> Int -> CExpression -> Bool
checkCallExpr fun params expr@CallExpr {}
  | isCallToFun fun expr =
    let (_, args) = CDefs.collectArgs expr
    in length args >= params
checkCallExpr fun params e = and [checkCallExpr fun params c | c <- childrenExpr e]