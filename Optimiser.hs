{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use lambda-case" #-}

module Optimiser where
import C
import qualified AbsLang as AL
import qualified NamedLang as NL
import qualified CLang as CL

import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Typeable
import System.IO
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe


removeCallParamEnv :: CStatement a -> Int -> Set.Set Int -> CStatement a
removeCallParamEnv (BindExpr t x i y) ifun s = BindExpr t (removeCallParamEnvExpr x ifun s) i (removeCallParamEnv y ifun s)
removeCallParamEnv (DefVar t i x) ifun s = DefVar t i (removeCallParamEnvExpr x ifun s)
removeCallParamEnv (UpdateVar t i x) ifun s = UpdateVar t i (removeCallParamEnvExpr x ifun s)
removeCallParamEnv (DefFun t i p body) _ s = DefFun t i p (removeCallParamEnv body i s)
removeCallParamEnv (Seq x y) ifun s = Seq (removeCallParamEnv x ifun s) (removeCallParamEnv y ifun s)
removeCallParamEnv (If c x y) ifun s = If (removeCallParamEnvExpr c ifun s) (removeCallParamEnv x ifun s) (removeCallParamEnv y ifun s)
removeCallParamEnv (While c x) ifun s = While (removeCallParamEnvExpr c ifun s) (removeCallParamEnv x ifun s)
removeCallParamEnv (Return x) ifun s = Return (removeCallParamEnvExpr x ifun s)
removeCallParamEnv x _ _ = x

removeCallParamEnvExpr :: CExpression a -> Int -> Set.Set Int -> CExpression a
removeCallParamEnvExpr (GetEnvField t envId varId) ifun s
    | envId == ifun =
        if ifun `elem` s
        then 
            -- trace ("removing " ++ show ifun ++ " " ++ show varId) $
            Var t varId
        else GetEnvField t envId varId
removeCallParamEnvExpr (CallExpr tf tx f x) ifun s =
    let (f', args) = collectArgs (CallExpr tf tx f x)
    in case f' of
        Var _ i -> 
            if i `elem` s
            then 
                let args' = case args of
                                (CArg _ (Val (EnvV _)) : rest) -> rest
                                _ -> args
                in rebuildCall tf f' (map (\(CArg t arg) -> CArg t (removeCallParamEnvExpr arg ifun s)) args')
            else CallExpr tf tx (removeCallParamEnvExpr f ifun s) (removeCallParamEnvExpr x ifun s)
        _ -> CallExpr tf tx (removeCallParamEnvExpr f ifun s) (removeCallParamEnvExpr x ifun s)
removeCallParamEnvExpr (Not x) ifun s = Not (removeCallParamEnvExpr x ifun s)
removeCallParamEnvExpr (Abs x) ifun s = Abs (removeCallParamEnvExpr x ifun s)
removeCallParamEnvExpr (Fst tp tr x) ifun s = Fst tp tr (removeCallParamEnvExpr x ifun s)
removeCallParamEnvExpr (Snd tp tr x) ifun s = Snd tp tr (removeCallParamEnvExpr x ifun s)
removeCallParamEnvExpr (IsEmpty t x) ifun s = IsEmpty t (removeCallParamEnvExpr x ifun s)
removeCallParamEnvExpr (HeadList t x) ifun s = HeadList t (removeCallParamEnvExpr x ifun s)
removeCallParamEnvExpr (TailList t x) ifun s = TailList t (removeCallParamEnvExpr x ifun s)
removeCallParamEnvExpr (Box t x) ifun s = Box t (removeCallParamEnvExpr x ifun s)
removeCallParamEnvExpr (Unbox t x) ifun s = Unbox t (removeCallParamEnvExpr x ifun s)
removeCallParamEnvExpr (CastExpr t x) ifun s = CastExpr t (removeCallParamEnvExpr x ifun s)
removeCallParamEnvExpr (LIntOp op x y) ifun s = LIntOp op (removeCallParamEnvExpr x ifun s) (removeCallParamEnvExpr y ifun s)
removeCallParamEnvExpr (LCmpOp op x y) ifun s = LCmpOp op (removeCallParamEnvExpr x ifun s) (removeCallParamEnvExpr y ifun s)
removeCallParamEnvExpr (LBoolOp op x y) ifun s = LBoolOp op (removeCallParamEnvExpr x ifun s) (removeCallParamEnvExpr y ifun s)
removeCallParamEnvExpr (Ternary t c x y) ifun s = Ternary t (removeCallParamEnvExpr c ifun s) (removeCallParamEnvExpr x ifun s) (removeCallParamEnvExpr y ifun s)
removeCallParamEnvExpr (ApplyClosure t f x) ifun s = ApplyClosure t (removeCallParamEnvExpr f ifun s) (removeCallParamEnvExpr x ifun s)
removeCallParamEnvExpr (ConsList t x y) ifun s = ConsList t (removeCallParamEnvExpr x ifun s) (removeCallParamEnvExpr y ifun s)
removeCallParamEnvExpr (Prod t x y) ifun s = Prod t (removeCallParamEnvExpr x ifun s) (removeCallParamEnvExpr y ifun s)
removeCallParamEnvExpr (IndexList t x y) ifun s = IndexList t (removeCallParamEnvExpr x ifun s) (removeCallParamEnvExpr y ifun s)
removeCallParamEnvExpr x _ _ = x

-- remove env params that are never used
removeUnusedParams :: CParams -> FunctionInfo -> (CParams, Bool)
removeUnusedParams [] _ = ([], False)
removeUnusedParams [CParam i t] _ = ([CParam i t], False)
removeUnusedParams [CParamEnv i] m = 
    if i `elem` envUses m 
    then ([CParamEnv i], False)
    else ([], True)
removeUnusedParams (i:is) m = 
    let (i', b) = removeUnusedParams [i] m
        (is', b') = removeUnusedParams is m
    in (i' ++ is', b || b')

removeEnvParam :: CStatement a -> CStatement b -> FunctionInfo -> State (Set.Set Int) (CStatement a)
removeEnvParam (DefFun t i p body) stmt _ =
    let r' = getFunctionInfo (DefFun t i p body) emptyFunctionInfo
        (p', changed) = removeUnusedParams p r'
    in if changed 
        then do
            -- trace ("removed env " ++ show i ++ " | " ++ showCStmt 0 Map.empty Map.empty body) $ 
            modify (Set.insert i)
            body' <- removeEnvParam body stmt r'
            return $ DefFun t i p' body'
        else do
            body' <- removeEnvParam body stmt r'
            return $ DefFun t i p' body'
removeEnvParam (Seq x y) stmt r = Seq <$> removeEnvParam x stmt r <*> removeEnvParam y stmt r
removeEnvParam x _ _ = return $ x

paramsHaveEnv :: Int -> CParams -> Bool
paramsHaveEnv _ [] = False
paramsHaveEnv i [CParamEnv i'] = i == i'
paramsHaveEnv _ [CParam _ _] = False
paramsHaveEnv i (a:as) = paramsHaveEnv i [a] || paramsHaveEnv i as

-- can only remove closure if it was actually alloced in this function, ontherwise it was passed as a param
-- removes envs that are only used locally (no alloc needed) and vars that are only used <= once

-- | not (Set.member envId (allocedEnvs r)) && not (paramsHaveEnv envId (funParams r)) = removeEnvsExpr (Var t varId) stmt r
        -- | Set.member envId (envUses r) = GetEnvField t envId varId
        -- | otherwise = removeEnvsExpr (Var t varId) stmt r


rewriteRemovedEnvs :: Set.Set Int -> CStatement a -> CStatement a
rewriteRemovedEnvs removed (BindExpr t x i y) = BindExpr t (rewriteRemovedEnvsExpr removed x) i (rewriteRemovedEnvs removed y)
rewriteRemovedEnvs removed (DefVar t i x) = DefVar t i (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvs removed (UpdateVar t i x) = UpdateVar t i (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvs removed (Seq x y) = Seq (rewriteRemovedEnvs removed x) (rewriteRemovedEnvs removed y)
rewriteRemovedEnvs removed (If c x y) = If (rewriteRemovedEnvsExpr removed c) (rewriteRemovedEnvs removed x) (rewriteRemovedEnvs removed y)
rewriteRemovedEnvs removed (While c x) = While (rewriteRemovedEnvsExpr removed c) (rewriteRemovedEnvs removed x)
rewriteRemovedEnvs removed (Return x) = Return (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvs removed (DefFun t i p body) = DefFun t i p (rewriteRemovedEnvs removed body)
rewriteRemovedEnvs removed (AllocEnv envId parentId directPs parentPs) =
    AllocEnv envId parentId
        (Map.map (\(CArg t x) -> CArg t (rewriteRemovedEnvsExpr removed x)) directPs)
        (Map.map (\(CArg t x) -> CArg t (rewriteRemovedEnvsExpr removed x)) parentPs)
rewriteRemovedEnvs _ x = x

rewriteRemovedEnvsExpr :: Set.Set Int -> CExpression a -> CExpression a
rewriteRemovedEnvsExpr removed (GetEnvField t envId varId)
    | Set.member envId removed = 
        -- trace ("    env" ++ show envId ++ "->" ++ " = var" ++ show varId)
        rewriteRemovedEnvsExpr removed (Var t varId)
    | otherwise = GetEnvField t envId varId
rewriteRemovedEnvsExpr removed (CallExpr tf tx f x) = CallExpr tf tx (rewriteRemovedEnvsExpr removed f) (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvsExpr removed (Not x) = Not (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvsExpr removed (Abs x) = Abs (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvsExpr removed (Fst tp tr x) = Fst tp tr (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvsExpr removed (Snd tp tr x) = Snd tp tr (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvsExpr removed (IsEmpty t x) = IsEmpty t (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvsExpr removed (HeadList t x) = HeadList t (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvsExpr removed (TailList t x) = TailList t (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvsExpr removed (Box t x) = Box t (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvsExpr removed (Unbox t x) = Unbox t (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvsExpr removed (CastExpr t x) = CastExpr t (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvsExpr removed (LIntOp op x y) = LIntOp op (rewriteRemovedEnvsExpr removed x) (rewriteRemovedEnvsExpr removed y)
rewriteRemovedEnvsExpr removed (LCmpOp op x y) = LCmpOp op (rewriteRemovedEnvsExpr removed x) (rewriteRemovedEnvsExpr removed y)
rewriteRemovedEnvsExpr removed (LBoolOp op x y) = LBoolOp op (rewriteRemovedEnvsExpr removed x) (rewriteRemovedEnvsExpr removed y)
rewriteRemovedEnvsExpr removed (Ternary t c x y) = Ternary t (rewriteRemovedEnvsExpr removed c) (rewriteRemovedEnvsExpr removed x) (rewriteRemovedEnvsExpr removed y)
rewriteRemovedEnvsExpr removed (ApplyClosure t f x) = ApplyClosure t (rewriteRemovedEnvsExpr removed f) (rewriteRemovedEnvsExpr removed x)
rewriteRemovedEnvsExpr removed (ConsList t x y) = ConsList t (rewriteRemovedEnvsExpr removed x) (rewriteRemovedEnvsExpr removed y)
rewriteRemovedEnvsExpr removed (Prod t x y) = Prod t (rewriteRemovedEnvsExpr removed x) (rewriteRemovedEnvsExpr removed y)
rewriteRemovedEnvsExpr removed (IndexList t x y) = IndexList t (rewriteRemovedEnvsExpr removed x) (rewriteRemovedEnvsExpr removed y)
rewriteRemovedEnvsExpr _ x = x

removeEnvs :: CStatement a -> CStatement a
removeEnvs (DefFun t i p body) =
    let r' = getFunctionInfo (DefFun t i p body) emptyFunctionInfo
        (body', removed) = removeEnvAllocs body (DefFun t i p body) r'
    in 
        -- trace ("removed " ++ show removed)
        DefFun t i p (rewriteRemovedEnvs removed body')
removeEnvs (Seq x y) = Seq (removeEnvs x) (removeEnvs y)
removeEnvs x = x

-- PASS 1: remove the AllocEnv statements that aren't needed, collect their ids
removeEnvAllocs :: CStatement a -> CStatement b -> FunctionInfo -> (CStatement a, Set.Set Int)
removeEnvAllocs (AllocEnv envId parentId directPs parentPs) _ r =
    if Set.member envId (escapedEnvs r)  -- escapes via closure/application: keep it
    then (AllocEnv envId parentId directPs parentPs, Set.empty)
    else (Skip, Set.singleton envId)
removeEnvAllocs (BindExpr t x i y) stmt r =
    let (y', removed) = removeEnvAllocs y stmt r
    in (BindExpr t x i y', removed)
removeEnvAllocs (DefVar t i x) _ _ = (DefVar t i x, Set.empty)
removeEnvAllocs (UpdateVar t i x) _ _ = (UpdateVar t i x, Set.empty)
removeEnvAllocs (DefFun t i p body) stmt _ =
    let r' = getFunctionInfo (DefFun t i p body) emptyFunctionInfo
        (body', removed) = removeEnvAllocs body stmt r'
    in (DefFun t i p body', removed)
removeEnvAllocs (Seq x y) stmt r =
    let (x', rx) = removeEnvAllocs x stmt r
        (y', ry) = removeEnvAllocs y stmt r
    in (Seq x' y', Set.union rx ry)
removeEnvAllocs (If c x y) stmt r =
    let (x', rx) = removeEnvAllocs x stmt r
        (y', ry) = removeEnvAllocs y stmt r
    in (If c x' y', Set.union rx ry)
removeEnvAllocs (While c x) stmt r =
    let (x', rx) = removeEnvAllocs x stmt r
    in (While c x', rx)
removeEnvAllocs (Return x) _ _ = (Return x, Set.empty)
removeEnvAllocs x _ _ = (x, Set.empty)

removeSingleVars :: CStatement a -> CStatement b -> FunctionInfo -> CStatement a
removeSingleVars (BindExpr t x i y) stmt r =
    case Map.lookup i (varUses r) of
        Just n | n <= 1 -> removeSingleVars y stmt r
        _ -> BindExpr t (removeSingleVarsExpr x stmt r) i (removeSingleVars y stmt r)
removeSingleVars (DefVar t i x) stmt r =
    case Map.lookup i (varUses r) of
        Just n | n <= 1 -> Skip
        _ -> DefVar t i (removeSingleVarsExpr x stmt r)
removeSingleVars (UpdateVar t i x) stmt r =
    case Map.lookup i (varUses r) of
        Just n | n <= 1 -> Skip
        _ -> UpdateVar t i (removeSingleVarsExpr x stmt r)
removeSingleVars (DefFun t i p body) stmt _ =
    let r' = getFunctionInfo (DefFun t i p body) emptyFunctionInfo
    in DefFun t i p (removeSingleVars body stmt r')
removeSingleVars (Seq x y) stmt r = Seq (removeSingleVars x stmt r) (removeSingleVars y stmt r)
removeSingleVars (If c x y) stmt r = If (removeSingleVarsExpr c stmt r) (removeSingleVars x stmt r) (removeSingleVars y stmt r)
removeSingleVars (While c x) stmt r = While (removeSingleVarsExpr c stmt r) (removeSingleVars x stmt r)
removeSingleVars (Return x) stmt r = Return (removeSingleVarsExpr x stmt r)
removeSingleVars x _ _ = x

removeSingleVarsExpr :: CExpression a -> CStatement b -> FunctionInfo -> CExpression a
removeSingleVarsExpr (Var t i) stmt r =
    case (Map.lookup i (varUses r), Map.lookup i (varDefs r)) of
        (Just n, Just (CArg _ x)) | n <= 1 -> removeSingleVarsExpr (unsafeCoerce x) stmt r
        _ -> Var t i
removeSingleVarsExpr (Val (ClosureV i)) stmt r =
    case (Map.lookup i (varUses r), Map.lookup i (varDefs r)) of
        (Just n, Just (CArg _ x)) | n <= 1 -> removeSingleVarsExpr (unsafeCoerce x) stmt r
        _ -> Val (ClosureV i)
removeSingleVarsExpr (CallExpr tf tx f x) stmt r = CallExpr tf tx (removeSingleVarsExpr f stmt r) (removeSingleVarsExpr x stmt r)
removeSingleVarsExpr (Not x) stmt r = Not (removeSingleVarsExpr x stmt r)
removeSingleVarsExpr (Abs x) stmt r = Abs (removeSingleVarsExpr x stmt r)
removeSingleVarsExpr (Fst tp tr x) stmt r = Fst tp tr (removeSingleVarsExpr x stmt r)
removeSingleVarsExpr (Snd tp tr x) stmt r = Snd tp tr (removeSingleVarsExpr x stmt r)
removeSingleVarsExpr (IsEmpty t x) stmt r = IsEmpty t (removeSingleVarsExpr x stmt r)
removeSingleVarsExpr (HeadList t x) stmt r = HeadList t (removeSingleVarsExpr x stmt r)
removeSingleVarsExpr (TailList t x) stmt r = TailList t (removeSingleVarsExpr x stmt r)
removeSingleVarsExpr (Box t x) stmt r = Box t (removeSingleVarsExpr x stmt r)
removeSingleVarsExpr (Unbox t x) stmt r = Unbox t (removeSingleVarsExpr x stmt r)
removeSingleVarsExpr (CastExpr t x) stmt r = CastExpr t (removeSingleVarsExpr x stmt r)
removeSingleVarsExpr (LIntOp op x y) stmt r = LIntOp op (removeSingleVarsExpr x stmt r) (removeSingleVarsExpr y stmt r)
removeSingleVarsExpr (LCmpOp op x y) stmt r = LCmpOp op (removeSingleVarsExpr x stmt r) (removeSingleVarsExpr y stmt r)
removeSingleVarsExpr (LBoolOp op x y) stmt r = LBoolOp op (removeSingleVarsExpr x stmt r) (removeSingleVarsExpr y stmt r)
removeSingleVarsExpr (Ternary t c x y) stmt r = Ternary t (removeSingleVarsExpr c stmt r) (removeSingleVarsExpr x stmt r) (removeSingleVarsExpr y stmt r)
removeSingleVarsExpr (ApplyClosure t f x) stmt r = ApplyClosure t (removeSingleVarsExpr f stmt r) (removeSingleVarsExpr x stmt r)
removeSingleVarsExpr (ConsList t x y) stmt r = ConsList t (removeSingleVarsExpr x stmt r) (removeSingleVarsExpr y stmt r)
removeSingleVarsExpr (Prod t x y) stmt r = Prod t (removeSingleVarsExpr x stmt r) (removeSingleVarsExpr y stmt r)
removeSingleVarsExpr (IndexList t x y) stmt r = IndexList t (removeSingleVarsExpr x stmt r) (removeSingleVarsExpr y stmt r)
removeSingleVarsExpr x _ _ = x



-- REMOVE CLOSURES

-- closure id, parent id, aplly to rewrite
-- turn the application of a heap allocated closure into the call of a stack allocated one
-- We call the fn stored in the closure directly with the env and its args
-- i is the id of the closureAlloc were getting rid of
    -- if we find the application of that closure we need to rewrite it to a callexpr
-- also we need to remove the cast for the apply
rewriteClosureUseExpr :: Int -> Int -> CExpression b -> CExpression b
rewriteClosureUseExpr i parentId (ApplyClosure targ f arg) = 
    let (f', args) = collectArgsApply (ApplyClosure targ f arg)
    in case f' of
        Val (ClosureV i') | i == i' -> 
            let args' = map (\(CArg t x) -> CArg t (rewriteClosureUseExpr i parentId x)) args
                envArg = CArg CTVoidPtr (Val (EnvV i))
            in rebuildCall CTVoidPtr (Var CTVoidPtr i) (envArg : args') -- call the closure function directly
        _ -> ApplyClosure targ (rewriteClosureUseExpr i parentId f) (rewriteClosureUseExpr i parentId arg)
rewriteClosureUseExpr i parentId (CastExpr t (ApplyClosure targ f arg)) = 
    let (f', _) = collectArgsApply (ApplyClosure targ f arg)
    in case f' of
        Val (ClosureV i') | i == i' -> rewriteClosureUseExpr i parentId (ApplyClosure targ f arg)
        _ -> CastExpr t (rewriteClosureUseExpr i parentId (ApplyClosure targ f arg))
rewriteClosureUseExpr i parentId (Ternary tp c t e) = Ternary tp (rewriteClosureUseExpr i parentId c) (rewriteClosureUseExpr i parentId t) (rewriteClosureUseExpr i parentId e)
rewriteClosureUseExpr i parentId (CallExpr tf tx f x) = CallExpr tf tx (rewriteClosureUseExpr i parentId f) (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr i parentId (CastExpr t x) = CastExpr t (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr i parentId (Box t x) = Box t (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr i parentId (Unbox t x) = Unbox t (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr i parentId (Not x) = Not (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr i parentId (Abs x) = Abs (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr i parentId (Fst tp tr x) = Fst tp tr (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr i parentId (Snd tp tr x) = Snd tp tr (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr i parentId (IsEmpty t x) = IsEmpty t (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr i parentId (HeadList t x) = HeadList t (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr i parentId (TailList t x) = TailList t (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr i parentId (LIntOp op x y) = LIntOp op (rewriteClosureUseExpr i parentId x) (rewriteClosureUseExpr i parentId y)
rewriteClosureUseExpr i parentId (LCmpOp op x y) = LCmpOp op (rewriteClosureUseExpr i parentId x) (rewriteClosureUseExpr i parentId y)
rewriteClosureUseExpr i parentId (LBoolOp op x y) = LBoolOp op (rewriteClosureUseExpr i parentId x) (rewriteClosureUseExpr i parentId y)
rewriteClosureUseExpr i parentId (ConsList t x y) = ConsList t (rewriteClosureUseExpr i parentId x) (rewriteClosureUseExpr i parentId y)
rewriteClosureUseExpr i parentId (Prod t x y) = Prod t (rewriteClosureUseExpr i parentId x) (rewriteClosureUseExpr i parentId y)
rewriteClosureUseExpr i parentId (IndexList t x y) = IndexList t (rewriteClosureUseExpr i parentId x) (rewriteClosureUseExpr i parentId y)
rewriteClosureUseExpr _ _ x = x

-- remove the alloc closures we don't need and the envs that have no direct params
rewriteClosureUse :: Int -> Int -> CStatement b -> CStatement b
rewriteClosureUse i parentId (Return x) = Return (rewriteClosureUseExpr i parentId x)
rewriteClosureUse i parentId (Seq x y) = Seq (rewriteClosureUse i parentId x) (rewriteClosureUse i parentId y)
rewriteClosureUse i parentId (DefVar t i' x) = DefVar t i' (rewriteClosureUseExpr i parentId x)
rewriteClosureUse i parentId (UpdateVar t i' x) = UpdateVar t i' (rewriteClosureUseExpr i parentId x)
rewriteClosureUse i parentId (BindExpr t x j y) = BindExpr t (rewriteClosureUseExpr i parentId x) j (rewriteClosureUse i parentId y)
rewriteClosureUse i parentId (If c t e) = If (rewriteClosureUseExpr i parentId c) (rewriteClosureUse i parentId t) (rewriteClosureUse i parentId e)
rewriteClosureUse i parentId (While c x) = While (rewriteClosureUseExpr i parentId c) (rewriteClosureUse i parentId x)
rewriteClosureUse _ _ x = x

-- top level pass, if we alloc a closure that never escapes the function we can keep it on the stack
-- collects a list of closures that are local to the function they are in
    -- it then removes all of these from the body of that function
removeClosureAllocs :: CStatement a -> FunctionInfo -> (CStatement a, [(Int, Int)])
removeClosureAllocs (AllocClosure i) g
    | i `elem` escapedClos g = (AllocClosure i, [])
    | otherwise = (Skip, [(i, i)])
removeClosureAllocs (Seq x y) g =
    let (x', r) = removeClosureAllocs x g
        (y', r') = removeClosureAllocs y g
    in (Seq x' y', r ++ r')
removeClosureAllocs (If cond x y) g =
    let (x', r) = removeClosureAllocs x g
        (y', r') = removeClosureAllocs y g
    in (If cond x' y', r ++ r')
removeClosureAllocs (While cond x) g =
    let (x', r) = removeClosureAllocs x g
    in (While cond x', r)
removeClosureAllocs (DefFun tret ifun ps x) _ =
    let (x', r) = removeClosureAllocs x (getFunctionInfo x emptyFunctionInfo)
        x'' = foldr (uncurry rewriteClosureUse) x' r
    in (DefFun tret ifun ps x'', r)
removeClosureAllocs (BindExpr t x i y) g =
    let (y', r) = removeClosureAllocs y g
    in (BindExpr t x i y', r)
removeClosureAllocs x _ = (x, [])





-- ****** INLINE FUNCTIONS




-- INLINING

endsInIf :: CStatement a -> Bool
endsInIf If {} = True
endsInIf (Seq _ y) = endsInIf y
endsInIf (BindExpr _ _ _ y) = endsInIf y
endsInIf _ = False

-- Single inlining pass, finds list of functions that are safe to inline (called exactly once)
-- It then tries to inline all of these functions
-- It returns a list of the functions that were removed so I can get rid of them later
inlinePass :: [CStatement a] -> CStatement a -> (CStatement a, [Int])
inlinePass defs body =
    let globalInfo = getGlobalInfo body emptyGlobalInfo
        safeToInline = Map.keys $ Map.filter (== 1) $ Map.filterWithKey
            (\i _ -> case findFunDef i defs of
                Just DefFun {} -> True
                _ -> False) (functionCallsGlobal globalInfo)
    in foldr (\i (b, removed) ->
            let (b', didInline) = inlineOne i defs b
           in if didInline then (b', i : removed) else (b, removed)
        ) (body, []) safeToInline

-- replace return statement inside both branches of an if statement
-- so that we can then inline that if statement
-- using id of inlined function as fresh var to hold result
replaceReturn :: Int -> CType -> CStatement a -> CStatement a
replaceReturn i t (Return x) = UpdateVar t i x
replaceReturn i t (Seq x y) = Seq x (replaceReturn i t y)
replaceReturn i t (BindExpr t' x j y) = BindExpr t' x j (replaceReturn i t y)
replaceReturn i t (If c x y) = If c (replaceReturn i t x) (replaceReturn i t y)
replaceReturn i t (While c x) = While c (replaceReturn i t x)
replaceReturn _ _ x = x

-- Inline all calls to function i throughout body
-- if the function body ends in if we need to handle two returns
    -- replace the returns with an accumulator var
inlineOne :: Int -> [CStatement a] -> CStatement a -> (CStatement a, Bool)
inlineOne i defs body =
    case findFunDef i defs of
        Just (DefFun tret ifun params fbody) ->
            if endsInIf fbody then (
                let retExpr = Var tret ifun
                    bodyNoRet = Seq (DefVar tret ifun (Val (defaultVal tret))) (replaceReturn ifun tret fbody)
                    body' = inlineCallsTo i params bodyNoRet retExpr body
                in (body', True))
            else (
                let retExpr = findFirstReturn fbody
                    bodyNoRet = removeFirstReturn fbody
                    body' = inlineCallsTo i params bodyNoRet retExpr body
                in (body', True))
        _ -> (body, False)

-- takes a pair of cparam and arg and turns them into a var definition
-- so that the inlined body can still use its arguments
inlineArgs :: (CParam, CArg) -> CStatement a -> CStatement a
inlineArgs (param, CArg _ arg) acc =
    case (param, arg) of
        (CParam ip tp, _) -> Seq (DefVar tp ip arg) acc
        (CParamEnv ip, Val (EnvV ip'))
            | ip' == ip -> acc -- do not redefine env vars which are already defined, we don't want Env66* env66 = env66;
            | otherwise -> Seq (DefVar (CTPtr CTVoid) ip arg) acc
        (x, y) -> error ("mismatch arg and param" ++ show x ++ " | " ++ showCExpression y Map.empty)

-- Replace all CallExpr (Var i) args with inlined body
-- Replace ternary with if so that we can add the pre work
inlineCallsTo :: Int -> CParams -> CStatement a -> CExpression a -> CStatement a -> CStatement a
inlineCallsTo i params fbodyNoRet retExpr = goStmt
  where
    goStmt (Seq x y) = Seq (goStmt x) (goStmt y)
    goStmt (DefFun t j ps b) = DefFun t j ps (goStmt b)
    goStmt (Return x) =
        let (pre, x') = goExpr x
        in Seq pre (Return x')
    goStmt (BindExpr t x j y) =
        let (pre, x') = goExpr x
        in Seq pre (BindExpr t x' j (goStmt y))
    goStmt (If c x y) =
        let (pre, c') = goExpr c
        in Seq pre (If c' (goStmt x) (goStmt y))
    goStmt (While c x) =
        let (pre, c') = goExpr c
        in Seq pre (While c' (goStmt x))
    goStmt (DefVar t j x) =
        let (pre, x') = goExpr x
        in Seq pre (DefVar t j x')
    goStmt (UpdateVar t j x) =
        let (pre, x') = goExpr x
        in Seq pre (UpdateVar t j x')
    goStmt x = x

    goExpr :: CExpression b -> (CStatement a, CExpression b)
    goExpr expr@(CallExpr tf tx f x) =
        let (func, args) = collectArgs expr
        in case func of
            Var _ j | j == i -> -- called function is equal to the one we're inlining
                let funArgs = take (length params) args
                    bindings = foldr inlineArgs Skip (zip params funArgs)
                    pre = Seq bindings fbodyNoRet
                in (unsafeCoerce pre, unsafeCoerce retExpr)
            _ ->
                let (px, x') = goExpr x
                    (pf, f') = goExpr f
                in (Seq pf px, CallExpr tf tx f' x')
    goExpr (Not x) = let (p, x') = goExpr x in (p, Not x')
    goExpr (Abs x) = let (p, x') = goExpr x in (p, Abs x')
    goExpr (IsEmpty t x) = let (p, x') = goExpr x in (p, IsEmpty t x')
    goExpr (HeadList t x) = let (p, x') = goExpr x in (p, HeadList t x')
    goExpr (TailList t x) = let (p, x') = goExpr x in (p, TailList t x')
    goExpr (Fst tp tr x) = let (p, x') = goExpr x in (p, Fst tp tr x')
    goExpr (Snd tp tr x) = let (p, x') = goExpr x in (p, Snd tp tr x')
    goExpr (CastExpr t x)= let (p, x') = goExpr x in (p, CastExpr t x')
    goExpr (Unbox t x) = let (p, x') = goExpr x in (p, Unbox t x')
    goExpr (Box t x) = let (p, x') = goExpr x in (p, Box t x')
    goExpr (Ternary tp c t e) =
        let (pc, c') = goExpr c
            (pt, t') = goExpr t
            (pe, e') = goExpr e
        in (Seq pc (Seq pt pe), Ternary tp c' t' e')
    goExpr (ConsList t x y) =
        let (px, x') = goExpr x
            (py, y') = goExpr y
        in (Seq px py, ConsList t x' y')
    goExpr (LIntOp op x y) =
        let (px, x') = goExpr x
            (py, y') = goExpr y
        in (Seq px py, LIntOp op x' y')
    goExpr (LCmpOp op x y) =
        let (px, x') = goExpr x
            (py, y') = goExpr y
        in (Seq px py, LCmpOp op x' y')
    goExpr (LBoolOp op x y) =
        let (px, x') = goExpr x
            (py, y') = goExpr y
        in (Seq px py, LBoolOp op x' y')
    goExpr (ApplyClosure tx f x) =
        let (pf, f') = goExpr f
            (px, x') = goExpr x
        in (Seq pf px, ApplyClosure tx f' x')
    goExpr x = (Skip, x)

-- Keep inlining until nothing changes, bool indicates if anything was removed
inlineUntilFixed :: CStatement a -> (CStatement a, Bool)
inlineUntilFixed body =
    let (body', removed) = inlinePass (getDefs body) body
    in if null removed
       then (body', False)
       else let body'' = removeDeadFuns removed body'
                (body''', removed') = inlineUntilFixed body''
            in (body''', not (null removed) || removed')

-- pass list of called funs
removeDeadFuns :: [Int] -> CStatement a -> CStatement a
removeDeadFuns removedFuns def@(DefFun _ ifun _ _) =
    if ifun `elem` removedFuns then Skip else def -- still used
removeDeadFuns m (Seq x y) = Seq (removeDeadFuns m x) (removeDeadFuns m y)
removeDeadFuns _ x = x


cleanSkip :: CStatement a -> CStatement a
cleanSkip (Seq Skip y) = cleanSkip y
cleanSkip (Seq y Skip) = cleanSkip y
cleanSkip (Seq x y) = Seq (cleanSkip x) (cleanSkip y)
cleanSkip (If cond x y) = If cond (cleanSkip x) (cleanSkip y)
cleanSkip (While cond x) = While cond (cleanSkip x)
cleanSkip (DefFun tret ifun params y) = DefFun tret ifun params (cleanSkip y)
cleanSkip x = x

-- the only ones that need cast are fst and snd because they return void*
removeCast :: CExpression a -> CExpression b -> CExpression a
removeCast fallback x = case x of
    Fst{} -> fallback
    Snd{} -> fallback
    expr -> unsafeCoerce expr

-- REMOVE USELESS LOGIC AND CASTS (i.e. making a list out of the head and tail of another list)
removeUselessExpr :: CExpression a -> CExpression a
removeUselessExpr (CastExpr t x) = removeCast (CastExpr t x) (removeUselessExpr x)
removeUselessExpr (Unbox t x) = removeCast (Unbox t x) (removeUselessExpr x)
removeUselessExpr (ConsList t x y) =
    let x' = stripWrap x
        y' = stripWrap y
    in case (x', y') of
        (HeadList _ l1, TailList _ l2) | eqCExpr l1 l2 -> unsafeCoerce l1
        _ -> ConsList t (removeUselessExpr x) (removeUselessExpr y)
removeUselessExpr (LIntOp op x y) = LIntOp op (removeUselessExpr x) (removeUselessExpr y)
removeUselessExpr (LCmpOp op x y) = LCmpOp op (removeUselessExpr x) (removeUselessExpr y)
removeUselessExpr (Ternary t c x y) = Ternary t (removeUselessExpr c) (removeUselessExpr x) (removeUselessExpr y)
removeUselessExpr (CallExpr tf tx f x) = CallExpr tf tx (removeUselessExpr f) (removeUselessExpr x)
removeUselessExpr (ApplyClosure t f x) = ApplyClosure t (removeUselessExpr f) (removeUselessExpr x)
removeUselessExpr (Prod t x y) = Prod t (removeUselessExpr x) (removeUselessExpr y)
removeUselessExpr (Fst tp tr x) = Fst tp tr (removeUselessExpr x)
removeUselessExpr (Snd tp tr x) = Snd tp tr (removeUselessExpr x)
removeUselessExpr (IsEmpty t x) = IsEmpty t (removeUselessExpr x)
removeUselessExpr (HeadList t x) = HeadList t (removeUselessExpr x)
removeUselessExpr (TailList t x) = TailList t (removeUselessExpr x)
removeUselessExpr (IndexList t x y) = IndexList t (removeUselessExpr x) (removeUselessExpr y)
removeUselessExpr (Not x) = Not (removeUselessExpr x)
removeUselessExpr (Box t x) = Box t (removeUselessExpr x)
removeUselessExpr x = x

removeUselessStmt :: CStatement a -> CStatement a
removeUselessStmt (Return x) = Return (removeUselessExpr x)
removeUselessStmt (Seq x y) = Seq (removeUselessStmt x) (removeUselessStmt y)
removeUselessStmt (BindExpr t x i y) = BindExpr t (removeUselessExpr x) i (removeUselessStmt y)
removeUselessStmt (If c x y) = If (removeUselessExpr c) (removeUselessStmt x) (removeUselessStmt y)
removeUselessStmt (While c x) = While (removeUselessExpr c) (removeUselessStmt x)
removeUselessStmt (DefFun t i ps x) = DefFun t i ps (removeUselessStmt x)
removeUselessStmt (DefVar t i x) = DefVar t i (removeUselessExpr x)
removeUselessStmt (UpdateVar t i x) = UpdateVar t i (removeUselessExpr x)
removeUselessStmt (AllocEnv envId parentId directPs parentPs) =
    let directPs' = Map.map (\(CArg t x) -> CArg t (removeUselessExpr x)) directPs
        parentPs' = Map.map (\(CArg t x) -> CArg t (removeUselessExpr x)) parentPs
    in AllocEnv envId parentId directPs' parentPs'
removeUselessStmt x = x



-- Common Subexpression elimination

-- replaces all var defs of form v2 = v3 or v2 = env5
applyAliases :: CStatement a -> Map.Map Int CArg -> CStatement a
applyAliases (DefVar t i x) m = DefVar t i (replaceVarAssignment x m)
applyAliases (BindExpr t x i y) m = BindExpr t (replaceVarAssignment x m) i (applyAliases y m)
applyAliases (DefFun tret ifun param body) m = DefFun tret ifun param (applyAliases body m)
applyAliases (Seq x y) m = Seq (applyAliases x m) (applyAliases y m)
applyAliases (If cond x y) m = If (replaceVarAssignment cond m) (applyAliases x m) (applyAliases y m)
applyAliases (While cond x) m = While (replaceVarAssignment cond m) (applyAliases x m)
applyAliases (Return x) m = Return (replaceVarAssignment x m)
applyAliases (UpdateVar tx i x) m = UpdateVar tx i (replaceVarAssignment x m)
applyAliases (AllocEnv envId parentId directPs parentPs) m =
    let directPs' = Map.map (\(CArg t x) -> CArg t (replaceVarAssignment x m)) directPs
        parentPs' = Map.map (\(CArg t x) -> CArg t (replaceVarAssignment x m)) parentPs
    in AllocEnv envId parentId directPs' parentPs'
applyAliases x _ = x

-- returns bool true if an alias was eliminated (something changed)
eliminateAliases :: CStatement a -> (CStatement a, Bool)
eliminateAliases stmt =
    let info = getGlobalInfo stmt emptyGlobalInfo
        m = aliases info
        stmt' = applyAliases stmt m
        stmt'' = removeEnvs stmt'
        -- stmt''' = removeSingleVars stmt'' stmt'' emptyFunctionInfo
    in 
        -- trace ("aliases " ++ show m) $
        if stmt == stmt''
        then (stmt'', False)
        else eliminateAliases stmt''

replaceVarAssignment :: CExpression a -> Map.Map Int CArg -> CExpression a
replaceVarAssignment (Var t i) m =
    case Map.lookup i m of
        Just (CArg _ n) -> replaceVarAssignment (unsafeCoerce n) m
        Nothing -> Var t i
replaceVarAssignment (GetEnvField t structId fieldId) m =
    case Map.lookup structId m of
        Just (CArg _ (Var _ newId)) -> replaceVarAssignment (GetEnvField t newId fieldId) m
        Just (CArg _ (Val (EnvV newId))) -> replaceVarAssignment (GetEnvField t newId fieldId) m
        _ -> GetEnvField t structId fieldId
replaceVarAssignment (Not x) m = Not (replaceVarAssignment x m)
replaceVarAssignment (Abs x) m = Abs (replaceVarAssignment x m)
replaceVarAssignment (LIntOp op x y) m = LIntOp op (replaceVarAssignment x m) (replaceVarAssignment y m)
replaceVarAssignment (LCmpOp op x y) m = LCmpOp op (replaceVarAssignment x m) (replaceVarAssignment y m)
replaceVarAssignment (LBoolOp op x y) m = LBoolOp op (replaceVarAssignment x m) (replaceVarAssignment y m)
replaceVarAssignment (Ternary tp x y z) m = Ternary tp (replaceVarAssignment x m) (replaceVarAssignment y m) (replaceVarAssignment z m)
replaceVarAssignment (CallExpr tf tx x y) m = CallExpr tf tx (replaceVarAssignment x m) (replaceVarAssignment y m)
replaceVarAssignment (Prod t x y) m = Prod t (replaceVarAssignment x m) (replaceVarAssignment y m)
replaceVarAssignment (Fst tp tr x) m = Fst tp tr (replaceVarAssignment x m)
replaceVarAssignment (Snd tp tr x) m = Snd tp tr (replaceVarAssignment x m)
replaceVarAssignment (HeadList t x) m = HeadList t (replaceVarAssignment x m)
replaceVarAssignment (TailList t x) m = TailList t (replaceVarAssignment x m)
replaceVarAssignment (IsEmpty t x) m = IsEmpty t (replaceVarAssignment x m)
replaceVarAssignment (IndexList t i x) m = IndexList t i (replaceVarAssignment x m)
replaceVarAssignment (ConsList t x y) m = ConsList t (replaceVarAssignment x m) (replaceVarAssignment y m)
replaceVarAssignment (Box t x) m = Box t (replaceVarAssignment x m)
replaceVarAssignment (Unbox t x) m = Unbox t (replaceVarAssignment x m)
replaceVarAssignment (ApplyClosure t x y) m = ApplyClosure t (replaceVarAssignment x m) (replaceVarAssignment y m)
replaceVarAssignment (CastExpr t x) m = CastExpr t (replaceVarAssignment x m)
replaceVarAssignment expr _ = expr




-- STACK ALLOCATE PAIRS

data UseKind = OnlyFstSnd | BadUse deriving Eq

canBeByValueFun :: Int -> CStatement a -> Map.Map Int Bool -> Bool
canBeByValueFun ifun body varsByValue =
    let isAlwaysStored = returnIsAlwaysStored ifun body
        varsThatHoldReturn = getVarsThatHoldReturn ifun body
    in isAlwaysStored && all (\i -> case Map.lookup i varsByValue of
                                        Just True -> True
                                        _ -> False) varsThatHoldReturn

getValueFunsSet :: [Int] -> CStatement a -> Map.Map Int Bool -> Map.Map Int Bool
getValueFunsSet [] _ _ = Map.empty
getValueFunsSet (i:rest) body varsByValue =
    if canBeByValueFun i body varsByValue
    then
        case findFunDef i (getDefs body) of
            Just funDef ->
                let analysis = getFunctionInfo funDef emptyFunctionInfo
                    m' = Map.insert i True (getValueFunsSet rest body varsByValue)
                in foldr (`Map.insert` True) m' (escapedVars analysis)
            _ -> error "not valid definition"
    else getValueFunsSet rest body varsByValue

-- A pair is safe to put of stack iff every use of it is only with fst or snd
canBeByValue :: CStatement a -> Map.Map Int Bool
canBeByValue stmt =
    let pairLocals = collectPairLocals stmt   -- vars whose DefVar type is pair
        usage = scanUses stmt
        varsbyValue = Map.mapWithKey (\i _ -> Map.findWithDefault BadUse i usage == OnlyFstSnd) pairLocals
        funs = getFunsWithParams stmt
        funsByValue = getValueFunsSet (Map.keys funs) stmt varsbyValue
    in Map.unionWith (||) varsbyValue funsByValue

-- if a pair is used not as fst/snd anywhere it is 'bad'
mergeUse :: UseKind -> UseKind -> UseKind
mergeUse OnlyFstSnd OnlyFstSnd = OnlyFstSnd
mergeUse _ _ = BadUse

-- check pair usage
scanExpr :: CExpression a -> Map.Map Int UseKind
scanExpr (Fst _ _ (Var _ i)) = Map.singleton i OnlyFstSnd
scanExpr (Snd _ _ (Var _ i)) = Map.singleton i OnlyFstSnd
scanExpr (Var _ i) = Map.singleton i BadUse  -- bare use is bad
scanExpr (Fst _ _ e) = scanExpr e
scanExpr (Snd _ _ e) = scanExpr e
scanExpr (Not x) = scanExpr x
scanExpr (Abs x) = scanExpr x
scanExpr (LIntOp _ x y) = Map.unionWith mergeUse (scanExpr x) (scanExpr y)
scanExpr (LCmpOp _ x y) = Map.unionWith mergeUse (scanExpr x) (scanExpr y)
scanExpr (LBoolOp _ x y) = Map.unionWith mergeUse (scanExpr x) (scanExpr y)
scanExpr (Ternary _ c x y) = Map.unionWith mergeUse (scanExpr c) (Map.unionWith mergeUse (scanExpr x) (scanExpr y))
scanExpr (ConsList _ x y) = Map.unionWith mergeUse (scanExpr x) (scanExpr y)
scanExpr (Prod _ x y) = Map.unionWith mergeUse (scanExpr x) (scanExpr y)
scanExpr (HeadList _ e) = scanExpr e
scanExpr (TailList _ e) = scanExpr e
scanExpr (IsEmpty _ e) = scanExpr e
scanExpr (CallExpr _ _ f a) = Map.unionWith mergeUse (scanExpr f) (scanExpr a)
scanExpr (ApplyClosure _ f a) = Map.unionWith mergeUse (scanExpr f) (scanExpr a)
scanExpr (CastExpr _ e) = scanExpr e
scanExpr (Box _ e) = scanExpr e
scanExpr (Unbox _ e) = scanExpr e
scanExpr _ = Map.empty

scanUses :: CStatement a -> Map.Map Int UseKind
scanUses (DefVar _ _ e) = scanExpr e
scanUses (BindExpr _ e _ k) = Map.unionWith mergeUse (scanExpr e) (scanUses k)
scanUses (UpdateVar _ _ e) = scanExpr e
scanUses (Return e) = scanExpr e
scanUses (Seq x y) = Map.unionWith mergeUse (scanUses x) (scanUses y)
scanUses (If c x y) = Map.unionWith mergeUse (scanExpr c) (Map.unionWith mergeUse (scanUses x) (scanUses y))
scanUses (While c x) = Map.unionWith mergeUse (scanExpr c) (scanUses x)
scanUses (DefFun _ _ _ b) = scanUses b
scanUses _  = Map.empty

collectPairLocals :: CStatement a -> Map.Map Int CType
collectPairLocals (DefVar t i _)
    | isPair t = Map.singleton i t
collectPairLocals (BindExpr t _ i k)
    | isPair t = Map.insert i t (collectPairLocals k)
    | otherwise = collectPairLocals k
collectPairLocals (Seq x y) = Map.union (collectPairLocals x) (collectPairLocals y)
collectPairLocals (If _ x y) = Map.union (collectPairLocals x) (collectPairLocals y)
collectPairLocals (While _ x) = collectPairLocals x
collectPairLocals (DefFun _ _ params b) =
    Map.union (collectPairLocals b) (collectPairArgs params)
    where
        collectPairArgs [] = Map.empty
        collectPairArgs [CParam i t] = if isPair t then Map.insert i t Map.empty else Map.empty
        collectPairArgs [_] = Map.empty
        collectPairArgs (i:is) = Map.union (collectPairArgs [i]) (collectPairArgs is)
collectPairLocals _ = Map.empty

isPair :: CType -> Bool
isPair (CTPair _ _) = True
isPair (CTPtr (CTPair _ _)) = True
isPair _ = False



getTypeExpr :: CExpression a -> CType
getTypeExpr (HeadList t _) = t
getTypeExpr (TailList t _) = t
getTypeExpr (Var t _) = t
getTypeExpr (Not _) = CTBool
getTypeExpr (Abs _) = CTInt
getTypeExpr (LIntOp _ _ _) = CTInt
getTypeExpr (LCmpOp _ _ _) = CTBool
getTypeExpr (LBoolOp _ _ _) = CTBool
getTypeExpr (Ternary t _ _ _) = t
getTypeExpr (Prod t _ _) = t
getTypeExpr (Fst _ t _) = t
getTypeExpr (Snd _ t _) = t
getTypeExpr (EmptyList t) = t
getTypeExpr (ConsList t _ _) = t
getTypeExpr (IsEmpty _ _) = CTBool
getTypeExpr (IndexList t _ _) = t
getTypeExpr (ApplyClosure {}) = CTVoidPtr
getTypeExpr (GetEnvField t _ _) = t
getTypeExpr (CallExpr tf _ _ _) = tf
getTypeExpr (CastExpr t _) = t
getTypeExpr (Box t _) = t
getTypeExpr (Unbox t _) = t
getTypeExpr (Val _) = CTVoidPtr

-- a function return type can be demoted if every use of the return value of said function
-- is stored in a var that can also be demoted

-- returns true if the result of call this function is only ever stored in a var
    -- through defvars/updatevars/binds directly
returnIsAlwaysStoredExpr :: Int -> CExpression a -> Bool
returnIsAlwaysStoredExpr ifun expr@CallExpr{} =
    let (f', _) = collectArgs expr
    in case f' of
        Var _ i' | i' == ifun -> False
        _ -> True
returnIsAlwaysStoredExpr ifun (Not x) = returnIsAlwaysStoredExpr ifun x
returnIsAlwaysStoredExpr ifun (LIntOp _ x y) = returnIsAlwaysStoredExpr ifun x && returnIsAlwaysStoredExpr ifun y
returnIsAlwaysStoredExpr ifun (LCmpOp _ x y) = returnIsAlwaysStoredExpr ifun x && returnIsAlwaysStoredExpr ifun y
returnIsAlwaysStoredExpr ifun (Prod _ x y) = returnIsAlwaysStoredExpr ifun x && returnIsAlwaysStoredExpr ifun y
returnIsAlwaysStoredExpr ifun (ConsList _ x y) = returnIsAlwaysStoredExpr ifun x && returnIsAlwaysStoredExpr ifun y
returnIsAlwaysStoredExpr ifun (Fst _ _ y) = returnIsAlwaysStoredExpr ifun y
returnIsAlwaysStoredExpr ifun (Snd _ _ y) = returnIsAlwaysStoredExpr ifun y
returnIsAlwaysStoredExpr ifun (IsEmpty _ y) = returnIsAlwaysStoredExpr ifun y
returnIsAlwaysStoredExpr ifun (HeadList _ y) = returnIsAlwaysStoredExpr ifun y
returnIsAlwaysStoredExpr ifun (TailList _ y) = returnIsAlwaysStoredExpr ifun y
returnIsAlwaysStoredExpr ifun (CastExpr _ x) = returnIsAlwaysStoredExpr ifun x
returnIsAlwaysStoredExpr ifun (IndexList _ _ x) = returnIsAlwaysStoredExpr ifun x
returnIsAlwaysStoredExpr ifun (Box _ x) = returnIsAlwaysStoredExpr ifun x
returnIsAlwaysStoredExpr ifun (Unbox _ x) = returnIsAlwaysStoredExpr ifun x
returnIsAlwaysStoredExpr _ _ = True


returnIsAlwaysStored :: Int -> CStatement a -> Bool
returnIsAlwaysStored ifun (DefVar _ _ expr@CallExpr{}) =
    let (_, args) = collectArgs expr
    in all (\(CArg _ x) -> returnIsAlwaysStoredExpr ifun x) args
returnIsAlwaysStored ifun (DefVar _ _ x) = returnIsAlwaysStoredExpr ifun x
returnIsAlwaysStored ifun (UpdateVar _ _ expr@CallExpr{}) =
    let (_, args) = collectArgs expr
    in all (\(CArg _ x) -> returnIsAlwaysStoredExpr ifun x) args
returnIsAlwaysStored ifun (UpdateVar _ _ x) = returnIsAlwaysStoredExpr ifun x
returnIsAlwaysStored ifun (BindExpr _ expr@CallExpr{} _ y) =
    let (_, args) = collectArgs expr
    in all (\(CArg _ x) -> returnIsAlwaysStoredExpr ifun x) args && returnIsAlwaysStored ifun y
returnIsAlwaysStored ifun (BindExpr _ x _ y) = returnIsAlwaysStoredExpr ifun x && returnIsAlwaysStored ifun y
returnIsAlwaysStored ifun (Seq x y) = returnIsAlwaysStored ifun x && returnIsAlwaysStored ifun y
returnIsAlwaysStored ifun (If c x y) = returnIsAlwaysStoredExpr ifun c && returnIsAlwaysStored ifun x && returnIsAlwaysStored ifun y
returnIsAlwaysStored ifun (While c x) = returnIsAlwaysStoredExpr ifun c && returnIsAlwaysStored ifun x
returnIsAlwaysStored ifun (Return x) = returnIsAlwaysStoredExpr ifun x
returnIsAlwaysStored ifun (DefFun _ _ _ x) = returnIsAlwaysStored ifun x
returnIsAlwaysStored _ _ = True

-- get all the defvars/updatevars/binds that hold a return of that function
getVarsThatHoldReturn :: Int -> CStatement a -> Set.Set Int
getVarsThatHoldReturn ifun (DefVar _ i expr@CallExpr{}) =
    let (f', _) = collectArgs expr
    in case f' of
        Var _ i' | i' == ifun -> Set.insert i Set.empty
        _ -> Set.empty
getVarsThatHoldReturn ifun (UpdateVar _ i expr@CallExpr{}) =
    let (f', _) = collectArgs expr
    in case f' of
        Var _ i' | i' == ifun -> Set.insert i Set.empty
        _ -> Set.empty
getVarsThatHoldReturn ifun (BindExpr _ expr@CallExpr{} i y) =
    let (f', _) = collectArgs expr
    in case f' of
        Var _ i' | i' == ifun -> Set.insert i (getVarsThatHoldReturn ifun y)
        _ -> getVarsThatHoldReturn ifun y
getVarsThatHoldReturn ifun (Seq x y) = Set.union (getVarsThatHoldReturn ifun x) (getVarsThatHoldReturn ifun y)
getVarsThatHoldReturn ifun (If _ x y) = Set.union (getVarsThatHoldReturn ifun x) (getVarsThatHoldReturn ifun y)
getVarsThatHoldReturn ifun (While _ y) = getVarsThatHoldReturn ifun y
getVarsThatHoldReturn ifun (DefFun _ _ _ y) = getVarsThatHoldReturn ifun y
getVarsThatHoldReturn _ _ = Set.empty



-- put pairs on stack

-- turns pair pointer into just pair if it can be demoted
stripPairPtr :: Int -> CType -> Map.Map Int Bool -> CType
stripPairPtr i t m =
    case Map.lookup i m of
        Just True ->
            case t of
                (CTPtr (CTPair tl tr)) -> CTPair tl tr
                x -> x
        _ -> t

-- need to explicitly strip prod of its type because it does not know what variable its held in
demotePairs :: CStatement a -> Map.Map Int Bool -> Map.Map Int [Int] -> CStatement a
demotePairs (DefFun tret ifun params body) m funs = DefFun (stripPairPtr ifun tret m) ifun (demotePairsParams params) (demotePairs body m funs)
    where
        demotePairsParams :: CParams -> CParams
        demotePairsParams [] = []
        demotePairsParams [CParam i t] = [CParam i (stripPairPtr i t m)]
        demotePairsParams [x] = [x]
        demotePairsParams (i:is) = demotePairsParams [i] ++ demotePairsParams is
demotePairs (DefVar t i (Prod tp x y)) m funs = DefVar (stripPairPtr i t m) i (Prod (stripPairPtr i tp m) (demotePairsExpr x m funs) (demotePairsExpr y m funs))
demotePairs (DefVar t i (Val UnitV)) m _ =
    let t' = stripPairPtr i t m
    in DefVar t' i (Val (defaultVal t'))
demotePairs (DefVar t i x) m funs = 
    let t' = stripPairPtr i t m
        x' = demotePairsExpr x m funs
    in  if t == t' then DefVar t' i x'
        else case getTypeExpr x' of
            (CTPtr _) -> DefVar t' i (Unbox t' x')
            CTNode -> DefVar t' i (Unbox t' x')
            CTVoidPtr -> DefVar t' i (Unbox t' x')
            _ -> DefVar t' i x'
demotePairs (UpdateVar t i (Prod tp x y)) m funs = UpdateVar (stripPairPtr i t m) i (Prod (stripPairPtr i tp m) (demotePairsExpr x m funs) (demotePairsExpr y m funs))
demotePairs (UpdateVar t i x) m funs = UpdateVar (stripPairPtr i t m) i (demotePairsExpr x m funs)
demotePairs (If c x y) m funs = If (demotePairsExpr c m funs) (demotePairs x m funs) (demotePairs y m funs)
demotePairs (While c x) m funs = While (demotePairsExpr c m funs) (demotePairs x m funs)
demotePairs (BindExpr t x i y) m funs = BindExpr (stripPairPtr i t m) (demotePairsExpr x m funs) i (demotePairs y m funs)
demotePairs (Seq x y) m funs = Seq (demotePairs x m funs) (demotePairs y m funs)
demotePairs (Return x) m funs = Return (demotePairsExpr x m funs)
demotePairs x _ _ = x

demotePairsExpr :: CExpression a -> Map.Map Int Bool -> Map.Map Int [Int] -> CExpression a
demotePairsExpr (Var t i) m _ = Var (stripPairPtr i t m) i
demotePairsExpr (Not x) m funs = Not (demotePairsExpr x m funs)
demotePairsExpr (Abs x) m funs = Abs (demotePairsExpr x m funs)
demotePairsExpr (LIntOp op x y) m funs = LIntOp op (demotePairsExpr x m funs) (demotePairsExpr y m funs)
demotePairsExpr (LCmpOp op x y) m funs = LCmpOp op (demotePairsExpr x m funs) (demotePairsExpr y m funs)
demotePairsExpr (LBoolOp op x y) m funs = LBoolOp op (demotePairsExpr x m funs) (demotePairsExpr y m funs)
demotePairsExpr (Ternary t c x y) m funs = Ternary t (demotePairsExpr c m funs) (demotePairsExpr x m funs) (demotePairsExpr y m funs)
demotePairsExpr (Fst tp tr (Var tv i)) m _ = Fst (stripPairPtr i tp m) tr (Var tv i)
demotePairsExpr (Snd tp tr (Var tv i)) m _ = Snd (stripPairPtr i tp m) tr (Var tv i)
demotePairsExpr (Fst tp tr e) m funs = Fst tp tr (demotePairsExpr e m funs)
demotePairsExpr (Snd tp tr e) m funs = Snd tp tr (demotePairsExpr e m funs)
demotePairsExpr (Prod t x y) m funs = Prod t (demotePairsExpr x m funs) (demotePairsExpr y m funs)
demotePairsExpr (ConsList t x y) m funs = ConsList t (demotePairsExpr x m funs) (demotePairsExpr y m funs)
demotePairsExpr (HeadList t e) m funs = HeadList t (demotePairsExpr e m funs)
demotePairsExpr (TailList t e) m funs = TailList t (demotePairsExpr e m funs)
demotePairsExpr (IsEmpty t e) m funs = IsEmpty t (demotePairsExpr e m funs)
demotePairsExpr (IndexList t x y) m funs = IndexList t (demotePairsExpr x m funs) (demotePairsExpr y m funs)
demotePairsExpr (ApplyClosure t f a) m funs = ApplyClosure t (demotePairsExpr f m funs) (demotePairsExpr a m funs)
demotePairsExpr (CallExpr tf tx f a) m funs =
        let (f', args) = collectArgs (CallExpr tf tx f a)
        in case f' of
            Var _ i -> case Map.lookup i funs of
                            Just ids -> rebuildCall tf f' (demoteArgs args ids)
                            _ -> CallExpr tf tx (demotePairsExpr f m funs) (demotePairsExpr a m funs)
                where
                    demoteArgs :: [CArg] -> [Int] -> [CArg]
                    demoteArgs [] _ = []
                    demoteArgs [CArg targ (Prod tp l r)] [idProd] =
                        case Map.lookup idProd m of
                            Just True -> [CArg tp (Prod (stripPairPtr idProd tp m) (demotePairsExpr l m funs) (demotePairsExpr r m funs))]
                            _ -> [CArg targ (Prod tp l r)]
                    demoteArgs [CArg targ x] _ = [CArg targ (demotePairsExpr x m funs)]
                    demoteArgs (i:is) (id':id'') = demoteArgs [i] [id'] ++ demoteArgs is id''
            _ -> CallExpr tf tx (demotePairsExpr f m funs) (demotePairsExpr a m funs)
    -- CallExpr tf tx (demotePairsExpr f m funs) (demotePairsExpr a m funs)
demotePairsExpr (CastExpr t e) m funs = CastExpr t (demotePairsExpr e m funs)
demotePairsExpr (Box t e) m funs = Box t (demotePairsExpr e m funs)
demotePairsExpr (Unbox t e) m funs = Unbox t (demotePairsExpr e m funs)
demotePairsExpr x _ _ = x


-- ***** HELPERS ****

-- collects a map of each fun id with the ids of its params from the whole ast
getFunsWithParams :: CStatement a -> Map.Map Int [Int]
getFunsWithParams (DefFun _ ifun params _) =
    Map.insert ifun (paramsToListEnv params) Map.empty
getFunsWithParams (Seq x y) = Map.union (getFunsWithParams x) (getFunsWithParams y)
getFunsWithParams _ = Map.empty

-- returns the deffun of fun i from the list of defs
findFunDef :: Int -> [CStatement a] -> Maybe (CStatement a)
findFunDef _ [] = Nothing
findFunDef i (def@(DefFun _ ifun _ _) : rest) =
    if i == ifun then Just def
    else findFunDef i rest
findFunDef _ _ = error "not valid def"


stripBox :: CExpression a -> CExpression a
stripBox (Box _ x) = unsafeCoerce x
stripBox x         = x

-- compare cexpression just by string
eqCExpr :: CExpression a -> CExpression b -> Bool
eqCExpr x y = showCExpression x Map.empty == showCExpression y Map.empty

-- get a default value to init the new var before the if statement
defaultVal :: CType -> CValue a
defaultVal CTInt  = unsafeCoerce (IntV 0)
defaultVal CTBool = unsafeCoerce (BoolV False)
defaultVal (CTPair l r) =
    let l' = defaultVal l
    in unsafeCoerce (PairV (unsafeCoerce l') (unsafeCoerce defaultVal r))
defaultVal _ = unsafeCoerce UnitV



-- MAIN

keepOptimising :: CStatement a -> CStatement a
keepOptimising body =
    let (body', _) = removeClosureAllocs body emptyFunctionInfo
        (inlinedBody, _) = inlineUntilFixed body' -- inline
        (removedEnvParamBody, l) = runState (removeEnvParam inlinedBody inlinedBody emptyFunctionInfo) Set.empty
        removedEnvParamBody' = removeCallParamEnv removedEnvParamBody (-1) l

        -- remove aliases, local envs and vars that are used <= 1 times
        (elminatedBody, _) = eliminateAliases removedEnvParamBody'
        removedVars = removeSingleVars elminatedBody elminatedBody emptyFunctionInfo

        -- remove useless logic and casts
        removedUselessBody = removeUselessStmt removedVars
    in if body == removedUselessBody
        then removedUselessBody
        else keepOptimising removedUselessBody

helloRun :: Typeable a => String -> AL.Lang a -> IO ()
helloRun progName progCode = do
    let libName = "\n#include \"" ++ "../"  ++ "listLib.c\"\n"
    let progPath = "optimised/" ++ progName
    -- let libName = "\n#include \"listLib.c\"\n"
    -- let progPath = progName

    let (nl, fresh') = runState (NL.translate progCode) 0
        (clBase, fresh'') = runState (CL.translate nl) fresh'
        clOpt = CL.optimizeBindings clBase
        c = translate clOpt

    let (cbody0, closureEnv, _) = runLiftAndMerge True c fresh''
    let cbody = addBoxing cbody0 -- boxing values
    let strFunTypes = getStrFunTypes (getDefs cbody) Map.empty

    -- optimise
    let optimisedBody = keepOptimising cbody
    let finalBody' = demotePairs optimisedBody (canBeByValue optimisedBody) (getFunsWithParams optimisedBody)
    let finalBody = cleanSkip finalBody'
    let finalDefs = getDefs finalBody
    let finalMergeMap = Map.map length (getFunsWithParams finalBody)

    let pairTypes = execState (collectPairTypes finalBody) Set.empty

    putStrLn "\n--- Printing C ---"
    let imports =   "\n#include <stdbool.h>" ++
                    "\n#include <stdio.h>" ++
                    "\n#include <stdlib.h>" ++
                    "\n#include <stdint.h>" ++
                    libName

    -- printgetFunctionInfo finalDefs
    let globalInfo = getGlobalInfo finalBody emptyGlobalInfo
    let envStructs = foldr Seq Skip (map (`generateEnvStructs` closureEnv) (Set.toList (usedEnvs globalInfo)))
    let funDefs = showFunDefs finalDefs
    let (funPart, mainBody) = splitTopLevel finalBody
    let retExpr = findFirstReturn mainBody
    let mainBodyWithoutRet = removeFirstReturn mainBody
    let retImpl = showCExpression retExpr finalMergeMap
    let mainBodyImpl = showCStmt 1 finalMergeMap strFunTypes mainBodyWithoutRet
    let funImpl = showCStmt 0 finalMergeMap strFunTypes funPart

    let content =
            "\n// imports" ++ imports ++
            "\n// pair type defitions" ++ concatMap genPairDeclaration (Set.toList pairTypes) ++
            "\n// function defitions" ++ funDefs ++
            "\n\n// closure defitions" ++ showCStmt 0 Map.empty strFunTypes envStructs ++
            "\n// function implementations" ++ funImpl ++
            "\n// main\nint main(void) {" ++ mainBodyImpl ++
                    case show (typeRep mainBody) of
                        "Int" -> "\n  printInt("
                        "[Int]" -> "\n  printListInt("
                        _ -> error "cannot print"
            ++ retImpl ++ ");\n" ++ "  return 0;\n}\n"

    -- writing to file
    let fileName = "outputs/" ++ progPath ++ ".c"
    handle <- openFile fileName WriteMode
    hPutStrLn handle content
    hClose handle
    putStrLn $ "Successfully wrote to " ++ fileName

hello :: IO ()
hello = do
    let progsInt = [("gcdLangCall", AL.gcdLangCall), ("fibCall", AL.fibCall), ("sumListCall", AL.sumListCall), ("lenListCall", AL.lenListCall)]
    let progsList = [("mapListCall", AL.mapListCall), ("mergeSortCall", AL.mergeSortCall)]
    let progsQueen = [("nQueensCall", AL.nQueensCall)]

    mapM_ (uncurry helloRun) progsInt
    mapM_ (uncurry helloRun) progsList
    mapM_ (uncurry helloRun) progsQueen