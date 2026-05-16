{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Optimiser where
import C
import qualified AbsLang as AL
import qualified NamedLang as NL
import qualified CLang as CL

import Debug.Trace
import qualified Data.Map as Map
import Control.Monad.State
import Data.Typeable
import System.IO
import Unsafe.Coerce (unsafeCoerce)

-- remove boxing when not necessary


-- REMOVE CLOSURE ALLOCS
countClosureUses :: Int -> CStatement a -> Int
countClosureUses i (Return x) = countClosureUsesExpr i x
countClosureUses i (Seq x y) = countClosureUses i x + countClosureUses i y
countClosureUses i (BindExpr _ x _ y) = countClosureUsesExpr i (unsafeCoerce x) + countClosureUses i y
countClosureUses i (If c t e) = countClosureUsesExpr i (unsafeCoerce c) + countClosureUses i t + countClosureUses i e
countClosureUses i (While c x) = countClosureUsesExpr i (unsafeCoerce c) + countClosureUses i x
countClosureUses _ _ = 0

countClosureUsesExpr :: Int -> CExpression a -> Int
countClosureUsesExpr i (Val (ClosureV j)) = if i == j then 1 else 0
countClosureUsesExpr i (Var _ j) = if i == j then 1 else 0
countClosureUsesExpr i (ApplyClosure f x) = countClosureUsesExpr i (unsafeCoerce f) + countClosureUsesExpr i (unsafeCoerce x)
countClosureUsesExpr i (CallExpr f x) = countClosureUsesExpr i (unsafeCoerce f) + countClosureUsesExpr i (unsafeCoerce x)
countClosureUsesExpr i (Ternary c t e) = countClosureUsesExpr i (unsafeCoerce c) + countClosureUsesExpr i (unsafeCoerce t) + countClosureUsesExpr i (unsafeCoerce e)
countClosureUsesExpr i (CastExpr _ f) = countClosureUsesExpr i f
countClosureUsesExpr _ _                      = 0

-- rewrite the single use of closure i to a direct call using envVar
rewriteClosureUse :: Int -> CStatement b -> CStatement b
rewriteClosureUse i (Return x) = Return (rewriteClosureUseExpr i (unsafeCoerce x))
rewriteClosureUse i (Seq (AllocClosure j) y)
    | i == j = rewriteClosureUse i y
rewriteClosureUse i (Seq x y) = Seq (rewriteClosureUse i x) (rewriteClosureUse i y)
rewriteClosureUse i (BindExpr t (x :: CExpression a) j y) =
    BindExpr t (rewriteClosureUseExpr i (unsafeCoerce x) :: CExpression a) j (rewriteClosureUse i y)
rewriteClosureUse i (If c t e) = If (rewriteClosureUseExpr i (unsafeCoerce c)) (rewriteClosureUse i t) (rewriteClosureUse i e)
rewriteClosureUse i (While c x) = While (rewriteClosureUseExpr i (unsafeCoerce c)) (rewriteClosureUse i x)
rewriteClosureUse _ x = x

rewriteApply :: Int -> CExpression a -> CExpression a
rewriteApply i (ApplyClosure f (arg :: CExpression b)) =
    case rewriteApply i (unsafeCoerce f) :: CExpression a of
        call@CallExpr{} -> unsafeCoerce $ CallExpr (call :: CExpression (b -> Int)) arg
        f' -> unsafeCoerce $ ApplyClosure f' (arg :: CExpression b)
rewriteApply i (Val (ClosureV i'))
    | i == i' = unsafeCoerce $ CallExpr
        (Var CTVoidPtr i :: CExpression (Int -> Int))
        (Val (EnvV i) :: CExpression Int)
rewriteApply _ x = x

rewriteClosureUseExpr :: Int -> CExpression b -> CExpression b
-- rewriteClosureUseExpr i (ApplyClosure f (arg :: CExpression arg)) =
--     let (f', args) = collectArgsApply (ApplyClosure f arg)
--     in case f' of
--         (Val (ClosureV i')) | i == i' ->
--             trace ("ARGS = " ++ show i ++ " | " ++ showCArgs args) $
--             unsafeCoerce $ rebuildCall (CallExpr 
--                     (Var CTVoidPtr i :: CExpression (Int -> Int)) 
--                     (Val (EnvV i) :: CExpression Int)) args
--         _ -> unsafeCoerce $ ApplyClosure 
--             (rewriteClosureUseExpr i (unsafeCoerce f))
--             (rewriteClosureUseExpr i (unsafeCoerce arg) :: CExpression arg)
rewriteClosureUseExpr i (ApplyClosure f (arg :: CExpression arg)) =
    unsafeCoerce $ rewriteApply i (ApplyClosure f arg)
rewriteClosureUseExpr i (Ternary (c :: CExpression Bool) (t :: CExpression b) (e :: CExpression b)) =
    unsafeCoerce $ Ternary
        (rewriteClosureUseExpr i (unsafeCoerce c) :: CExpression Bool)
        (rewriteClosureUseExpr i (unsafeCoerce t) :: CExpression b)
        (rewriteClosureUseExpr i (unsafeCoerce e) :: CExpression b)
rewriteClosureUseExpr i (CallExpr (f :: CExpression (a -> b)) x) =
    unsafeCoerce $ CallExpr
        (rewriteClosureUseExpr i (unsafeCoerce f) :: CExpression (a -> b))
        (rewriteClosureUseExpr i (unsafeCoerce x))
rewriteClosureUseExpr i (CastExpr t x) =
    unsafeCoerce $ CastExpr t (rewriteClosureUseExpr i (unsafeCoerce x))
rewriteClosureUseExpr _ x = x

-- top level pass
removeClosureAllocs :: CStatement a -> (CStatement a, [Int])
removeClosureAllocs (Seq (AllocClosure i) rest)
    | countClosureUses i rest == 1 =
        -- trace (if i == 63 then "REMOVE " ++ show  i else "") $
        let rest' = rewriteClosureUse i rest
            (x', r) = removeClosureAllocs rest'
        in (x', i : r)
    | otherwise =
        -- trace (if i == 63 then "REMOVE NOT " ++ show i ++ " | " ++ show (countClosureUses i rest) else "") $
        let (x', r) = removeClosureAllocs rest
        in (Seq (AllocClosure i) x', r)
removeClosureAllocs (Seq (AllocEnv i implId directPs parentPs) rest)
    | countClosureUses i rest == 1 =
        -- trace ("REMOVE1 " ++ show i ++ " | " ++ show (countClosureUses i rest)) $
        let rest' = rewriteClosureUse i rest
            (x', r) = removeClosureAllocs rest'
        in (Seq (AllocEnv i implId directPs parentPs) x', i : r)
    | otherwise =
        -- trace (if i == 63 then "REMOVE NOT1 " ++ show i ++ " | " ++ show (countClosureUses i rest) ++ " \n " ++ showCStmt 0 Map.empty Map.empty Map.empty rest else "") $
        let (x', r) = removeClosureAllocs rest
        in (Seq (AllocEnv i implId directPs parentPs) x', r)
removeClosureAllocs (Seq x y) =
    let (x', r) = removeClosureAllocs x
        (y', r') = removeClosureAllocs y
    in (Seq x' y', r ++ r')
removeClosureAllocs (If cond x y) =
    let (x', r) = removeClosureAllocs x
        (y', r') = removeClosureAllocs y
    in (If cond x' y', r ++ r')
removeClosureAllocs (While cond x) =
    let (x', r) = removeClosureAllocs x
    in (While cond x', r)
removeClosureAllocs (DefFun tret ifun ps x) =
    let (x', r) = removeClosureAllocs x
    in (DefFun tret ifun ps x', r)
removeClosureAllocs (BindExpr t x i y) =
    let (y', r) = removeClosureAllocs y
    in (BindExpr t x i y', r)
removeClosureAllocs x = (x, [])

-- INLINE FUNCTIONS
countFunctionCallsExpr :: CExpression a -> Map.Map Int Int -> Map.Map Int Int
countFunctionCallsExpr (CallExpr f x) m =
    case f of
        (Var _ i) -> countFunctionCallsExpr x (Map.insertWith (+) i 1 m)
        _ -> countFunctionCallsExpr x m
countFunctionCallsExpr (Not x) m = countFunctionCallsExpr x m
countFunctionCallsExpr (LIntOp _ x y) m = Map.unionWith (+) (countFunctionCallsExpr x m) (countFunctionCallsExpr y m)
countFunctionCallsExpr (LCmpOp _ x y) m = Map.unionWith (+) (countFunctionCallsExpr x m) (countFunctionCallsExpr y m)
countFunctionCallsExpr (Ternary x y z) m = Map.unionWith (+) (Map.unionWith (+) (countFunctionCallsExpr x m) (countFunctionCallsExpr y m)) (countFunctionCallsExpr z m)
countFunctionCallsExpr (Prod x y) m = Map.unionWith (+) (countFunctionCallsExpr x m) (countFunctionCallsExpr y m)
countFunctionCallsExpr (Fst x) m = countFunctionCallsExpr x m
countFunctionCallsExpr (Snd x) m = countFunctionCallsExpr x m
countFunctionCallsExpr (IsEmpty x) m = countFunctionCallsExpr x m
countFunctionCallsExpr (HeadList x) m = countFunctionCallsExpr x m
countFunctionCallsExpr (TailList x) m = countFunctionCallsExpr x m
countFunctionCallsExpr (IndexList x y) m = Map.unionWith (+) (countFunctionCallsExpr x m) (countFunctionCallsExpr y m)
countFunctionCallsExpr (ConsList x y) m = Map.unionWith (+) (countFunctionCallsExpr x m) (countFunctionCallsExpr y m)
countFunctionCallsExpr _ m = m

countFunctionCalls :: CStatement a -> Map.Map Int Int -> Map.Map Int Int
countFunctionCalls (DefFun _ _ _ body) m = countFunctionCalls body m
countFunctionCalls (Return x) m = countFunctionCallsExpr x m
countFunctionCalls (DefVar _ _ x) m = countFunctionCallsExpr x m
countFunctionCalls (UpdateVar _ x) m = countFunctionCallsExpr x m
countFunctionCalls (BindExpr _ x _ y) m = Map.unionWith (+) (countFunctionCallsExpr x m) (countFunctionCalls y m)
countFunctionCalls (While x y) m = Map.unionWith (+) (countFunctionCallsExpr x m) (countFunctionCalls y m)
countFunctionCalls (Seq x y) m = Map.unionWith (+) (countFunctionCalls x m) (countFunctionCalls y m)
countFunctionCalls (If x y z) m = Map.unionWith (+) (Map.unionWith (+) (countFunctionCallsExpr x m) (countFunctionCalls y m)) (countFunctionCalls z m)
countFunctionCalls _ m = m

getFun :: Int -> [CStatement a] -> Maybe (CStatement a)
getFun _ [] = Nothing
getFun i (def@(DefFun _ ifun _ _) : rest) =
    if i == ifun then Just def
    else getFun i rest
getFun _ _ = error "not valid def"

inlineFunsExpr :: Int -> [CStatement a] -> Map.Map Int Int -> CExpression a -> (CStatement a, CExpression a, [Int])
inlineFunsExpr ifun defs callMap (CallExpr f x) =
    case f of
        (Var _ i) | i /= ifun ->
            -- trace ("\nINLINING CALL VAR " ++ show i) $
            case Map.lookup i callMap of
                Just n | n <= 3 -> -- at most 3 calls
                    case getFun i defs of
                        Just (DefFun _ ifun1 [CParam ip tp] body) ->
                            -- trace ("\nINLINING " ++ show i ++ " | calls = " ++ show n) $
                            let (xPre, x', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
                                ct = fromTypeRep (typeRep tp)
                                (body', r') = inlineFuns ifun1 defs callMap body
                                bodyWithoutRet = removeFirstReturn body'
                                retExpr = findFirstReturn body'
                                inlined = Seq xPre (BindExpr ct x' ip bodyWithoutRet)
                            in
                                -- trace ("\nRET " ++ showCExpression retExpr Map.empty) $
                                (inlined, retExpr, i : (r ++ r') )
                        _ -> error "cannot find funcimpl"
                _ -> let (xPre, x', removed) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
                     in (xPre, CallExpr f (unsafeCoerce x'), removed)
        _ ->
            -- trace ("\nINLINING CALL NOT VAR " ++ showCExpression test Map.empty ) $
            let (xPre, x', removed) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
             in (xPre, CallExpr f (unsafeCoerce x'), removed)
inlineFunsExpr ifun defs callMap (Ternary c t e) =
    let (cp, c', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce c)
        (tp, t', r') = inlineFunsExpr ifun defs callMap t
        (ep, e', r'') = inlineFunsExpr ifun defs callMap e
    in (Seq cp (Seq tp ep), Ternary (unsafeCoerce c') t' e', r ++ r' ++ r'')
inlineFunsExpr ifun defs callMap (Not x) =
    let (p, x', r) = inlineFunsExpr ifun defs callMap x
    in (p, Not x', r)
inlineFunsExpr ifun defs callMap (HeadList x) =
    let (p, x', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
    in (p, HeadList (unsafeCoerce x'), r)
inlineFunsExpr ifun defs callMap (TailList x) =
    let (p, x', r) = inlineFunsExpr ifun defs callMap x
    in (p, TailList x', r)
inlineFunsExpr ifun defs callMap (IsEmpty (x :: CExpression [a])) =
    let (p, x', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
    in (p, IsEmpty (unsafeCoerce x' :: CExpression [a]), r)
inlineFunsExpr ifun defs callMap (Fst (x :: CExpression (a, b))) =
    let (p, x', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
    in (p, unsafeCoerce $ Fst (unsafeCoerce x' :: CExpression (a, b)), r)
inlineFunsExpr ifun defs callMap (Snd (x :: CExpression (a, b))) =
    let (p, x', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
    in (p, unsafeCoerce $ Snd (unsafeCoerce x' :: CExpression (a, b)), r)
inlineFunsExpr ifun defs callMap (LIntOp op x y) =
    let (xp, x', r) = inlineFunsExpr ifun defs callMap x
        (yp, y', r') = inlineFunsExpr ifun defs callMap y
    in (Seq xp yp, LIntOp op x' y', r ++ r')
inlineFunsExpr ifun defs callMap (LCmpOp op x y) =
    let (xp, x', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
        (yp, y', r') = inlineFunsExpr ifun defs callMap (unsafeCoerce y)
    in (Seq xp yp, LCmpOp op (unsafeCoerce x') (unsafeCoerce y'), r ++ r')
inlineFunsExpr ifun defs callMap (Prod x y) =
    let (xp, x', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
        (yp, y', r') = inlineFunsExpr ifun defs callMap (unsafeCoerce y)
    in (Seq xp yp, Prod (unsafeCoerce x') (unsafeCoerce y'), r ++ r')
inlineFunsExpr ifun defs callMap (ConsList x y) =
    let (xp, x', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
        (yp, y', r') = inlineFunsExpr ifun defs callMap y
    in (Seq xp yp, ConsList (unsafeCoerce x') (unsafeCoerce y'), r ++ r')
inlineFunsExpr ifun defs callMap (IndexList x y) =
    let (xp, x', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
        (yp, y', r') = inlineFunsExpr ifun defs callMap (unsafeCoerce y)
    in (Seq xp yp, IndexList (unsafeCoerce x') (unsafeCoerce y'), r ++ r')
inlineFunsExpr ifun defs callMap (CastExpr t x) =
    let (p, x', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
    in (p, CastExpr t x', r)
inlineFunsExpr ifun defs callMap (ApplyClosure (f :: CExpression fa) (x :: CExpression b)) =
    let (fp, f', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce f)
        (xp, x', r') = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
    in (Seq fp xp, unsafeCoerce $ ApplyClosure (unsafeCoerce f' :: CExpression fa) (unsafeCoerce x' :: CExpression b), r ++ r')
inlineFunsExpr _ _ _ x = (Skip, x, [])

-- current function, all fun defs, map of calls
inlineFuns :: Int -> [CStatement a] -> Map.Map Int Int  -> CStatement a -> (CStatement a, [Int])
inlineFuns _ defs callMap (DefFun tret ifun params body) =
    let (body', r) = inlineFuns ifun defs callMap body
    in (DefFun tret ifun params body', r)
inlineFuns ifun defs callMap (Return x) =
    let (pre, x', r) = inlineFunsExpr ifun defs callMap x
    in (Seq pre (Return x'), r)
inlineFuns ifun defs callMap (BindExpr t x i y) =
    let (pre, x', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
        (y', r') = inlineFuns ifun defs callMap y
        rebind = unsafeCoerce $ BindExpr t (unsafeCoerce x' :: CExpression Int) i y'
    in (Seq pre rebind, r ++ r')
inlineFuns ifun defs callMap (Seq x y) =
    let (x', r) = inlineFuns ifun defs callMap x
        (y', r') = inlineFuns ifun defs callMap y
    in (Seq x' y', r ++ r')
inlineFuns ifun defs callMap (If c x y) =
    let (pre, c', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce c)
        (x', r') = inlineFuns ifun defs callMap x
        (y', r'') = inlineFuns ifun defs callMap y
    in (Seq pre (If (unsafeCoerce c') x' y'), r ++ r' ++ r'')
inlineFuns ifun defs callMap (While c x) =
    let (pre, c', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce c)
        (x', r') = inlineFuns ifun defs callMap x
    in (Seq pre (While (unsafeCoerce c') x'), r ++ r')
inlineFuns ifun defs callMap (DefVar t i x) =
    let (pre, x', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
    in (Seq pre (DefVar t i (unsafeCoerce x' :: CExpression Int)), r)
inlineFuns ifun defs callMap (UpdateVar i x) =
    let (pre, x', r) = inlineFunsExpr ifun defs callMap (unsafeCoerce x)
    in( Seq pre (UpdateVar i (unsafeCoerce x' :: CExpression Int)),r)
inlineFuns _ _ _ x = (x, [])




-- Dead Code Elimination

removeDefFromList :: Int -> [CStatement a] -> [CStatement a]
removeDefFromList _ [] = []
removeDefFromList i (def@(DefFun _ ifun _ _) : rest)
    | i == ifun = rest
    | otherwise = def : removeDefFromList i rest
removeDefFromList _ _ = error "not fun"

-- pass list of removed funs
removeDeadFuns :: [Int] -> [CStatement a] -> CStatement a -> (CStatement a, [CStatement a])
removeDeadFuns removedFuns defs def@(DefFun _ ifun _ _) =
    if ifun `elem` removedFuns then (Skip, removeDefFromList ifun defs)
    else (def, defs) -- still used
removeDeadFuns m d (Seq x y) =
    let (x', d') = removeDeadFuns m d x
        (y', d'') = removeDeadFuns m d' y
    in (Seq x' y', d'')
removeDeadFuns _ d x = (x, d)




-- Function unrolling

-- int fact_iter(int n, int acc) {
--     while (1) {
--         if (n <= 1) return acc;
--         acc = acc * n;
--         n = n - 1;
--         // loop instead of recursive call
--     }
-- }

findReturn :: CStatement a -> Maybe (CExpression a)
findReturn (Return x) = Just x
findReturn (BindExpr _ _ _ y) = findReturn y
findReturn (Seq _ y) = findReturn y
findReturn (If _ x _) = findReturn x
findReturn (While _ x) = findReturn x
findReturn _ = Nothing

isTailRecursive :: Int -> CStatement a -> Bool
isTailRecursive ifun body =
    trace ("RETURN " ++ show ifun ++ " ||| " ++ case findReturn body of
        Just n -> showCExpression n Map.empty
        Nothing -> "nothing") $
    case findReturn body of
        Just (Ternary _ t e) -> hasTailCall ifun t || hasTailCall ifun e
        Just expr -> hasTailCall ifun expr
        Nothing   -> False

hasTailCall :: Int -> CExpression a -> Bool
hasTailCall ifun (CallExpr f _) = outermostVar f == Just ifun
hasTailCall ifun (Ternary _ t e) = hasTailCall ifun t || hasTailCall ifun e
hasTailCall _ _ = False

unrollFunctions :: CStatement a -> CStatement a
unrollFunctions (DefFun tret ifun params body) =
    if isTailRecursive ifun body then
        trace ("UNROLLING " ++ show ifun ++ " | " ++ showCStmt 0 Map.empty Map.empty Map.empty body) $
        DefFun tret ifun params (unrollFunctions body)
    else
        trace ("NO TAIL " ++ show ifun) $
        DefFun tret ifun params (unrollFunctions body)
unrollFunctions (Seq x y) = Seq (unrollFunctions x) (unrollFunctions y)
unrollFunctions s = s


-- Common Subexpression elimination



hello :: IO ()
hello = do
    let progsInt = [("gcdLangCall", AL.gcdLangCall), ("fibCall", AL.fibCall), ("sumListCall", AL.sumListCall), ("lenListCall", AL.lenListCall)]
    let progsList = [("mapListCall", AL.mapListCall), ("mergeSortCall", AL.mergeSortCall)]
    let (progName, progCode) = progsList !! 1
    let libName = "\n#include \"" ++ "listLib.c\"\n"
    let progPath = progName

    let (nl, c') = NL.translate 0 progCode
        (clBase, _) = runState (CL.translate nl) c'
        (clOpt, newBinds) = CL.optimizeBindings clBase Map.empty
        clOptRepl = CL.replaceVarBindingStmt clOpt newBinds
        c = translate clOptRepl

    putStrLn "--- Translating to CLang ---"
    putStrLn $ CL.showCStmt 0 clBase

    putStrLn "\n--- Merging Lambdas ---"
    let (merged, mergedMap) = mergeLambdas c c Map.empty
    putStrLn $ showCStmt 0 mergedMap Map.empty Map.empty merged

    -- putStrLn $ showCStmt 0 mergedMap Map.empty Map.empty merged
    -- let cbody = unrollFunctions cbody1

    let (cbody, closureEnv, liftenv, _, defs) = lambdaLift merged
    let strFunTypes = getStrFunTypes defs Map.empty

    -- putStrLn "\n--- Inlining funs ---"
    -- putStrLn $ showCStmt 0 mergedMap closureEnv strFunTypes cbody
    -- let callMap = countFunctionCalls cbody Map.empty
    -- let (cbody', removedFuns) = inlineFuns (-1) defs callMap cbody
    -- let (cbody'', defs') = removeDeadFuns removedFuns defs cbody'
    -- print callMap

    putStrLn "\n--- Removing Closure Allocs ---"
    let (cbody''', removedClosures) = removeClosureAllocs cbody
    print mergedMap
    let mergedMap' = foldr (\i m ->
                        let current = Map.findWithDefault 1 i m
                        in Map.insert i (current + 1) m
                    ) mergedMap removedClosures
    print mergedMap'
    -- putStrLn $ showCStmt 0 mergedMap closureEnv strFunTypes cbody'

    putStrLn "\n--- Printing C ---"
    let imports =   "\n#include <stdbool.h>" ++
                    "\n#include <stdio.h>" ++
                    "\n#include <stdlib.h>" ++
                    "\n#include <stdint.h>" ++
                    libName
    let closureStructs = generateClosureStructs (Map.toList liftenv)
    -- let funDefs = showFunDefs defs'
    let funDefs = showFunDefs defs
    let (funPart, mainBody) = splitTopLevel cbody'''
    let retExpr = findFirstReturn mainBody
    let mainBodyWithoutRet = removeFirstReturn mainBody

    let retImpl = showCExpression retExpr mergedMap'
    let mainBodyImpl = showCStmt 1 mergedMap' closureEnv strFunTypes mainBodyWithoutRet
    let funImpl = showCStmt 0 mergedMap' closureEnv strFunTypes funPart

    let content =
            "\n// imports" ++ imports ++
            "\n// function defitions" ++ funDefs ++
            "\n\n// closure defitions" ++ showCStmt 0 Map.empty Map.empty strFunTypes closureStructs ++
            "\n// function implementations" ++ funImpl ++
            "\n// main\nint main(void) {" ++ mainBodyImpl ++
                    case show (typeRep mainBody) of
                        "Int" -> "\n  printInt("
                        "[Int]" -> "\n  printList("
                        _ -> error "cannot print"
            ++ retImpl ++ ");\n" ++ "  return 0;\n}\n"

    -- writing to file
    let fileName = "outputs/" ++ progPath ++ ".c"
    handle <- openFile fileName WriteMode
    hPutStrLn handle content
    hClose handle
    putStrLn $ "Successfully wrote to " ++ fileName


    -- putStrLn funImpl
