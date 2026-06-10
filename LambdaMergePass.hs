{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LambdaMergePass where

import CDefs

-- for a function (int), given the amount of params, check that every call site has at least that many applications
checkCallExpr :: Int -> Int -> CExpression a -> Bool
checkCallExpr fun params expr =
    let (f, args) = CDefs.collectArgs expr
    in case f of
        Var _ i | i == fun -> length args >= params
        _ -> True

checkCallStmt :: Int -> Int -> CStatement a -> Bool
checkCallStmt fun params stmt = case stmt of
    Return e -> checkCallExpr fun params e
    Seq x y -> checkCallStmt fun params x && checkCallStmt fun params y
    If c t e -> checkCallExpr fun params c && checkCallStmt fun params t && checkCallStmt fun params e
    BindExpr _ e _ s -> checkCallExpr fun params e && checkCallStmt fun params s
    DefFun _ _ _ b -> checkCallStmt fun params b
    While c b -> checkCallExpr fun params c && checkCallStmt fun params b
    DefVar _ _ b -> checkCallExpr fun params b
    UpdateVar _ _ b -> checkCallExpr fun params b
    _  -> True

-- return merged and map of functions to their new number of params
-- the whole program unchanged, the current stmt, map of changed params
mergeLambdas :: CStatement b -> CStatement a -> CStatement a
mergeLambdas prog (DefFun tret ifun params body) =
    case body of
        (Seq (DefFun tret1 ifun1 params1 body1) (Return (Var _ i))) ->
            let newParams = params ++ params1
                canMerge = checkCallStmt ifun (length newParams) prog
            in  if canMerge && ifun1 == i
                then mergeLambdas prog (DefFun tret1 ifun newParams body1)
                else DefFun tret ifun params (mergeLambdas prog body)
        _ -> DefFun tret ifun params (mergeLambdas prog body)
mergeLambdas prog (Seq x y) = Seq (mergeLambdas prog x) (mergeLambdas prog y)
mergeLambdas prog (BindExpr t x i y) = BindExpr t x i (mergeLambdas prog y)
mergeLambdas prog (If c x y) = If c (mergeLambdas prog x) (mergeLambdas prog y)
mergeLambdas prog (While x y) = While x (mergeLambdas prog y)
mergeLambdas _ stmt = stmt
