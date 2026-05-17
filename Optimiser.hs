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
countClosureUsesExpr i (ApplyClosure f x) = countClosureUsesExpr i (unsafeCoerce f) + countClosureUsesExpr i (unsafeCoerce x)
countClosureUsesExpr i (CallExpr f x) =
    let (func, args) = collectArgs (CallExpr f x)
        funcCount = countClosureUsesExpr i (unsafeCoerce func)
        argsCount = foldr (\(CArg a) acc -> countClosureUsesExpr i (unsafeCoerce a) + acc) 0 args
    in funcCount + argsCount
countClosureUsesExpr i (Ternary c t e) = countClosureUsesExpr i (unsafeCoerce c) + countClosureUsesExpr i (unsafeCoerce t) + countClosureUsesExpr i (unsafeCoerce e)
countClosureUsesExpr i (CastExpr _ f) = countClosureUsesExpr i f
countClosureUsesExpr _ _ = 0

-- rewrite the single use of closure i to a direct call using envVar
rewriteClosureUse :: Int -> Int -> CStatement b -> CStatement b
rewriteClosureUse i parentId  (Return x) = Return (rewriteClosureUseExpr i parentId  (unsafeCoerce x))
rewriteClosureUse i parentId  (Seq (AllocClosure j) y)
    | i == j = rewriteClosureUse i parentId  y
rewriteClosureUse i parentId (Seq (AllocEnv j _ [] _) y)
    | i == j = rewriteClosureUse i parentId y
rewriteClosureUse i parentId  (Seq x y) = Seq (rewriteClosureUse i parentId  x) (rewriteClosureUse i parentId  y)
rewriteClosureUse i parentId  (BindExpr t (x :: CExpression a) j y) =
    BindExpr t (rewriteClosureUseExpr i parentId  (unsafeCoerce x) :: CExpression a) j (rewriteClosureUse i parentId  y)
rewriteClosureUse i parentId  (If c t e) = If (rewriteClosureUseExpr i parentId  (unsafeCoerce c)) (rewriteClosureUse i parentId  t) (rewriteClosureUse i parentId  e)
rewriteClosureUse i parentId  (While c x) = While (rewriteClosureUseExpr i parentId  (unsafeCoerce c)) (rewriteClosureUse i parentId  x)
rewriteClosureUse _ _ x = x

rewriteApply :: Int -> Int -> CExpression a -> CExpression a
rewriteApply i parentId  (ApplyClosure f (arg :: CExpression b)) =
    case rewriteApply i parentId  (unsafeCoerce f) :: CExpression a of
        call@CallExpr{} -> unsafeCoerce $ CallExpr (call :: CExpression (b -> Int)) arg
        f' -> unsafeCoerce $ ApplyClosure f' (arg :: CExpression b)
rewriteApply i parentId  (Val (ClosureV i'))
    | i == i' = unsafeCoerce $ CallExpr
        (Var CTVoidPtr i :: CExpression (Int -> Int))
        (Val (EnvV parentId) :: CExpression Int)
rewriteApply _ _ x = x

rewriteClosureUseExpr :: Int -> Int -> CExpression b -> CExpression b
rewriteClosureUseExpr i parentId  (ApplyClosure f (arg :: CExpression arg)) =
    unsafeCoerce $ rewriteApply i parentId (ApplyClosure f arg)
rewriteClosureUseExpr i parentId  (Ternary (c :: CExpression Bool) (t :: CExpression b) (e :: CExpression b)) =
    unsafeCoerce $ Ternary
        (rewriteClosureUseExpr i parentId  (unsafeCoerce c) :: CExpression Bool)
        (rewriteClosureUseExpr i parentId  (unsafeCoerce t) :: CExpression b)
        (rewriteClosureUseExpr i parentId  (unsafeCoerce e) :: CExpression b)
rewriteClosureUseExpr i parentId  (CallExpr (f :: CExpression (a -> b)) x) =
    unsafeCoerce $ CallExpr
        (rewriteClosureUseExpr i parentId  (unsafeCoerce f) :: CExpression (a -> b))
        (rewriteClosureUseExpr i parentId  (unsafeCoerce x))
rewriteClosureUseExpr i parentId  (CastExpr t x) =
    unsafeCoerce $ CastExpr t (rewriteClosureUseExpr i parentId  (unsafeCoerce x))
rewriteClosureUseExpr _ _ x = x

-- top level pass
removeClosureAllocs :: CStatement a -> (CStatement a, [Int])
removeClosureAllocs (Seq (AllocClosure i) rest)
    | countClosureUses i rest == 1 =
        let rest' = rewriteClosureUse i i rest
            (x', r) = removeClosureAllocs rest'
        in (x', i : r)
    | otherwise =
        let (x', r) = removeClosureAllocs rest
        in (Seq (AllocClosure i) x', r)
removeClosureAllocs (Seq (AllocEnv i implId directPs parentPs) rest)
    | countClosureUses i rest == 1 =
        if null directPs
            then
                let rest' = rewriteClosureUse i implId rest
                    (x', r) = removeClosureAllocs rest'
                in (x', i : r)
            else
                let rest' = rewriteClosureUse i i rest
                    (x', r) = removeClosureAllocs rest'
                in (Seq (AllocEnv i implId directPs parentPs) x', i : r)
    | otherwise =
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


-- ****** INLINE FUNCTIONS
countFunctionCallsExpr :: CExpression a -> Map.Map Int Int -> Map.Map Int Int
countFunctionCallsExpr (CallExpr f x) m =
    let (func, args) = collectArgs (CallExpr f x)
        m' = case func of
                Var _ i -> Map.insertWith (+) i 1 m
                _ -> m
    in foldr (\(CArg a) acc -> countFunctionCallsExpr (unsafeCoerce a) acc) m' args
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
countFunctionCallsExpr (ApplyClosure x y) m = Map.unionWith (+) (countFunctionCallsExpr x m) (countFunctionCallsExpr y m)
countFunctionCallsExpr (CastExpr _ y) m = countFunctionCallsExpr y m
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

endsInIf :: CStatement a -> Bool
endsInIf If {} = True
endsInIf (Seq _ y) = endsInIf y
endsInIf (BindExpr _ _ _ y) = endsInIf y
endsInIf _ = False

inlinePass :: [CStatement a] -> CStatement a -> (CStatement a, [Int])
inlinePass defs body =
    let callMap = countFunctionCalls body Map.empty
        safeToInline = Map.keys $ Map.filter (== 1) $ Map.filterWithKey
            (\i _ -> case getFun i defs of
                Just (DefFun _ _ _ b) -> not (endsInIf b)
                _ -> False) callMap
    in foldr (\i (b, removed) ->
            let (b', didInline) = inlineOne i defs b
            in if didInline then (b', i : removed) else (b, removed)
        ) (body, []) safeToInline

-- Inline all calls to function i throughout body
inlineOne :: Int -> [CStatement a] -> CStatement a -> (CStatement a, Bool)
inlineOne i defs body =
    case getFun i defs of
        Just (DefFun _ _ params fbody) ->
            let retExpr = findFirstReturn fbody
                bodyNoRet = removeFirstReturn fbody
                body' = inlineCallsTo i params bodyNoRet retExpr body
            in (body', True)
        _ -> (body, False)

-- Replace all CallExpr (Var i) args with inlined body
inlineCallsTo :: Int -> CParams -> CStatement a -> CExpression a -> CStatement a -> CStatement a
inlineCallsTo i params fbodyNoRet retExpr = goStmt
  where
    goStmt (Return x) = 
        case unsafeCoerce x :: CExpression Int of
            Ternary c t e ->
                let (pt, t') = goExpr (unsafeCoerce t)
                    (pe, e') = goExpr (unsafeCoerce e)
                    (pc, c') = goExpr (unsafeCoerce c)
                in Seq (unsafeCoerce pc) $ If (unsafeCoerce c')
                    (Seq (unsafeCoerce pt) (Return (unsafeCoerce t')))
                    (Seq (unsafeCoerce pe) (Return (unsafeCoerce e')))
            _ ->
                let (pre, x') = goExpr x
                in Seq pre (Return x')
    goStmt (Seq x y)          = Seq (goStmt x) (goStmt y)
    goStmt (BindExpr t x j y) = let (pre, x') = goExpr (unsafeCoerce x)
                                 in Seq pre (BindExpr t (unsafeCoerce x' :: CExpression Int) j (goStmt y))
    goStmt (If c x y)         = let (pre, c') = goExpr (unsafeCoerce c)
                                 in Seq pre (If (unsafeCoerce c') (goStmt x) (goStmt y))
    goStmt (DefFun t j ps b)  = DefFun t j ps (goStmt b)
    goStmt (While c x)         = let (pre, c') = goExpr (unsafeCoerce c)
                                 in Seq pre (While (unsafeCoerce c') (goStmt x))
    goStmt (DefVar t j (x :: CExpression a)) = let (pre, x') = goExpr (unsafeCoerce x)
                               in Seq pre (DefVar t j (unsafeCoerce x' :: CExpression a))
    goStmt (UpdateVar j (x :: CExpression a))= let (pre, x') = goExpr (unsafeCoerce x)
                                in Seq pre (UpdateVar j (unsafeCoerce x' :: CExpression a))
    goStmt x = x

    goExpr :: CExpression b -> (CStatement b, CExpression b)
    goExpr expr@(CallExpr _ _) =
        let (func, args) = collectArgs (unsafeCoerce expr)
        in case func of
            Var _ j | j == i ->
                let 
                    funArgs = take (length params) args
                    bindings = foldr 
                        (\pair acc -> case pair of
                            (CParam ip tp, CArg (arg :: CExpression c)) ->
                                let ct = fromTypeRep (typeRep tp)
                                in Seq (DefVar ct ip (arg :: CExpression c)) acc
                            (CParamEnv ip, CArg (arg :: CExpression c)) ->
                                case arg of
                                    (Val (EnvV ip'))
                                        | ip' == ip -> acc
                                        | otherwise -> Seq (DefVar CTVoidPtr ip (arg :: CExpression c)) acc
                                    _ -> error "mismatch arg and param"
                        ) Skip (zip params funArgs)
                    pre = Seq (unsafeCoerce bindings) (unsafeCoerce fbodyNoRet)
                in (unsafeCoerce pre, unsafeCoerce retExpr)
            _ -> (Skip, expr)
    goExpr (Ternary c t e) =
        let (pc, c') = goExpr (unsafeCoerce c)
            (pt, t') = goExpr t
            (pe, e') = goExpr e
        in (Seq (unsafeCoerce pc) (Seq pt pe), Ternary (unsafeCoerce c') t' e')
    goExpr (Not x) = let (p, x') = goExpr x in (p, Not x')
    goExpr (IsEmpty (x :: CExpression a))   = let (p, x') = goExpr (unsafeCoerce x) in (unsafeCoerce p, IsEmpty (unsafeCoerce x' :: CExpression a))
    goExpr (HeadList x)  = let (p, x') = goExpr (unsafeCoerce x) in (unsafeCoerce p, HeadList (unsafeCoerce x'))
    goExpr (TailList x)  = let (p, x') = goExpr x in (p, TailList x')
    goExpr (ConsList x y)=  let (px, x') = goExpr (unsafeCoerce x)
                                (py, y') = goExpr y
                        in (Seq (unsafeCoerce px) py, ConsList (unsafeCoerce x') y')
    goExpr (LIntOp op x y) =    let (px, x') = goExpr x
                                    (py, y') = goExpr y
                            in (Seq px py, LIntOp op x' y')
    goExpr (LCmpOp op x y) =    let (px, x') = goExpr (unsafeCoerce x)
                                    (py, y') = goExpr (unsafeCoerce y)
                            in (Seq (unsafeCoerce px) (unsafeCoerce py), unsafeCoerce $ LCmpOp op (unsafeCoerce x') (unsafeCoerce y'))
    goExpr (Fst (x :: CExpression (a,b))) = let (p, x') = goExpr (unsafeCoerce x) in (unsafeCoerce p, unsafeCoerce $ Fst (unsafeCoerce x' :: CExpression (a,b)))
    goExpr (Snd (x :: CExpression (a,b))) = let (p, x') = goExpr (unsafeCoerce x) in (unsafeCoerce p, unsafeCoerce $ Snd (unsafeCoerce x' :: CExpression (a,b)))
    goExpr (CastExpr t x)= let (p, x') = goExpr (unsafeCoerce x) in (unsafeCoerce p, CastExpr t x')
    goExpr (ApplyClosure (f :: CExpression a) (x :: CExpression b)) =
        let (pf, f') = goExpr (unsafeCoerce f)
            (px, x') = goExpr (unsafeCoerce x)
        in (Seq (unsafeCoerce pf) (unsafeCoerce px), unsafeCoerce $ ApplyClosure (unsafeCoerce f' :: CExpression a) (unsafeCoerce x' :: CExpression b))
    goExpr x = (Skip, x)

-- Keep inlining until nothing changes
inlineUntilFixed :: [CStatement a] -> CStatement a -> (CStatement a, [Int])
inlineUntilFixed defs body =
    let (body', removed) = inlinePass defs body
    in if null removed
       then (body', [])
       else
           let defs' = foldr removeDefFromList defs removed
               (body'', removed') = inlineUntilFixed defs' body'
           in (body'', removed ++ removed')



-- ****** Dead Code Elimination
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

replaceVarBinding :: CExpression a -> Map.Map Int CArg -> CExpression a
replaceVarBinding (Var t i) m =
    case Map.lookup i m of
        Just (CArg n) -> unsafeCoerce n
        Nothing       -> Var t i
replaceVarBinding (GetEnvField t structId fieldId) m =
    case Map.lookup structId m of
        Just (CArg (Val (EnvV newId))) -> GetEnvField t newId fieldId
        Just (CArg (Var _ newId))      -> GetEnvField t newId fieldId
        _                              -> GetEnvField t structId fieldId
replaceVarBinding (Not x) m = Not (replaceVarBinding x m)
replaceVarBinding (LIntOp op x y) m = LIntOp op (replaceVarBinding x m) (replaceVarBinding y m)
replaceVarBinding (LCmpOp op x y) m = LCmpOp op (replaceVarBinding x m) (replaceVarBinding y m)
replaceVarBinding (Ternary x y z) m = Ternary (replaceVarBinding x m) (replaceVarBinding y m) (replaceVarBinding z m)
replaceVarBinding (CallExpr x y) m = CallExpr (replaceVarBinding x m) (replaceVarBinding y m)
replaceVarBinding (Prod x y) m = Prod (replaceVarBinding x m) (replaceVarBinding y m)
replaceVarBinding (Fst x) m = Fst (replaceVarBinding x m)
replaceVarBinding (Snd x) m = Snd (replaceVarBinding x m)
replaceVarBinding (HeadList x) m = HeadList (replaceVarBinding x m)
replaceVarBinding (TailList x) m = TailList (replaceVarBinding x m)
replaceVarBinding (IsEmpty x) m = IsEmpty (replaceVarBinding x m)
replaceVarBinding (IndexList i x) m = IndexList i (replaceVarBinding x m)
replaceVarBinding (ConsList x y) m = ConsList (replaceVarBinding x m) (replaceVarBinding y m)
replaceVarBinding expr _ = expr

replaceVarBindingStmt :: CStatement a -> Map.Map Int CArg -> CStatement a
replaceVarBindingStmt (BindExpr t x i y) m =
  BindExpr t (replaceVarBinding x m) i (replaceVarBindingStmt y m)
replaceVarBindingStmt (Seq x y) m =
  Seq (replaceVarBindingStmt x m) (replaceVarBindingStmt y m)
replaceVarBindingStmt (If cond x y) m =
  let cond' = replaceVarBinding cond m
      x' = replaceVarBindingStmt x m
      y' = replaceVarBindingStmt y m
  in If cond' x' y'
replaceVarBindingStmt (While cond x) m =
  let cond' = replaceVarBinding cond m
      x' = replaceVarBindingStmt x m
  in While cond' x'
replaceVarBindingStmt (DefFun tret ifun param body) m =
  let body' = replaceVarBindingStmt body m
  in DefFun tret ifun param body'
replaceVarBindingStmt (Return x) m = Return (replaceVarBinding x m)
replaceVarBindingStmt (DefVar t i x) m = DefVar t i (replaceVarBinding x m)
replaceVarBindingStmt (UpdateVar i x) m = UpdateVar i (replaceVarBinding x m)
replaceVarBindingStmt x _ = x





hello :: IO ()
hello = do
    let progsInt = [("gcdLangCall", AL.gcdLangCall), ("fibCall", AL.fibCall), ("sumListCall", AL.sumListCall), ("lenListCall", AL.lenListCall)]
    let progsList = [("mapListCall", AL.mapListCall), ("mergeSortCall", AL.mergeSortCall)]
    let (progName, progCode) = progsList !! 1
    let libName = "\n#include \"" ++ "../"  ++ "listLib.c\"\n"
    let progPath = "inlined/" ++ progName

    let (nl, c') = NL.translate 0 progCode
        (clBase, _) = runState (CL.translate nl) c'
        (clOpt, newBinds) = CL.optimizeBindings clBase Map.empty
        clOptRepl = CL.replaceVarBindingStmt clOpt newBinds
        c = translate clOptRepl

    putStrLn "--- Translating to CLang ---"
    putStrLn $ CL.showCStmt 0 clBase

    putStrLn "\n--- Merging Lambdas ---"
    let (merged, mergedMap) = mergeLambdas c c Map.empty
    print mergedMap

    putStrLn "\n--- Lifting Lambdas ---"
    let (cbody, closureEnv, liftenv, _, defs) = lambdaLift merged
    putStrLn $ showCStmt 0 Map.empty Map.empty Map.empty cbody
    let strFunTypes = getStrFunTypes defs Map.empty

    putStrLn "\n--- Removing Closure Allocs ---"
    let (cbody', removedClosures) = removeClosureAllocs cbody
    print removedClosures
    let mergedMap' = foldr (\i m ->
                        let current = Map.findWithDefault 1 i m
                        in Map.insert i (current + 1) m
                    ) mergedMap removedClosures
    let defs' = map (fst . removeClosureAllocs) defs

    putStrLn "\n--- Inlining funs ---"
    let callMap = countFunctionCalls cbody' Map.empty
    let (cbody'', removedFuns) = inlineUntilFixed defs' cbody'
    let callMap' = countFunctionCalls cbody'' Map.empty
    let (cbody''', defs'') = removeDeadFuns removedFuns defs' cbody''
    -- let removed = Map.keys $ Map.filter (== 0) $ Map.intersectionWith (\old new -> new) callMap callMap'
    print callMap
    print callMap'
    print removedFuns

    let finalDefs = defs'' -- defs'
    let finalBody = cbody''' -- cbody
    let finalMergeMap = mergedMap'

    putStrLn "\n--- Printing C ---"
    let imports =   "\n#include <stdbool.h>" ++
                    "\n#include <stdio.h>" ++
                    "\n#include <stdlib.h>" ++
                    "\n#include <stdint.h>" ++
                    libName

    let closureStructs = generateClosureStructs (Map.toList liftenv)
    let funDefs = showFunDefs finalDefs
    let (funPart, mainBody) = splitTopLevel finalBody
    let retExpr = findFirstReturn mainBody
    let mainBodyWithoutRet = removeFirstReturn mainBody
    let retImpl = showCExpression retExpr finalMergeMap
    let mainBodyImpl = showCStmt 1 finalMergeMap closureEnv strFunTypes mainBodyWithoutRet
    let funImpl = showCStmt 0 finalMergeMap closureEnv strFunTypes funPart

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