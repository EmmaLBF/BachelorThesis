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
import Data.List (intercalate)

stripBox :: CExpression a -> CExpression a
stripBox (Box _ x) = unsafeCoerce x
stripBox x         = x

-- ESCAPE ANALYSIS

data EscapeResult = EscapeResult
    { escapedVars :: Set.Set Int   -- var ids that flow into heap
    , varUses :: Map.Map Int Int
    , varDefs :: Map.Map Int CArg
    , escapedEnvs :: Set.Set Int   -- env ids that outlive the frame
    , allocedEnvs :: Set.Set Int
    } deriving (Show)

emptyEscapeResult :: EscapeResult
emptyEscapeResult = EscapeResult Set.empty Map.empty Map.empty  Set.empty Set.empty

mergeEscape :: EscapeResult -> EscapeResult -> EscapeResult
mergeEscape a b = EscapeResult
    (Set.union (escapedVars a) (escapedVars b))
    (Map.unionWith max (varUses a) (varUses b))
    (Map.union (varDefs a) (varDefs b))
    (Set.union (escapedEnvs a) (escapedEnvs b))
    (Set.union (allocedEnvs a) (allocedEnvs b))

countVarUses :: CExpression a -> EscapeResult -> EscapeResult
countVarUses (Var _ i) r = r { varUses = Map.insertWith (+) i 1 (varUses r) }
countVarUses (Not x) m = countVarUses x m
countVarUses (Fst _ x) m = countVarUses x m
countVarUses (Snd _ x) m = countVarUses x m
countVarUses (IsEmpty _ x) m = countVarUses x m
countVarUses (HeadList _ x) m = countVarUses x m
countVarUses (TailList _ x) m = countVarUses x m
countVarUses (CastExpr _ x) m = countVarUses x m
countVarUses (Box _ x) m = countVarUses x m
countVarUses (Unbox _ x) m = countVarUses x m
countVarUses (Ternary _ _ t e) m = countVarUses t (countVarUses e m)
countVarUses (ConsList _ x y) m = countVarUses x (countVarUses y m)
countVarUses (Prod _ _ x y) m = countVarUses x (countVarUses y m)
countVarUses (LIntOp _ x y) m = countVarUses x (countVarUses y m)
countVarUses (LCmpOp _ x y) m = countVarUses x (countVarUses y m)
countVarUses (CallExpr _ _ f x) m = countVarUses f (countVarUses x m)
countVarUses (ApplyClosure _ f x) m = countVarUses f (countVarUses x m)
countVarUses (IndexList _ x y) m = countVarUses x (countVarUses y m)
countVarUses _ m = m

-- (vars, envs)
getReturnVars :: CExpression a -> EscapeResult -> EscapeResult
getReturnVars (Var _ i) r = r { escapedVars = Set.insert i (escapedVars r), varUses = Map.insertWith (+) i 1 (varUses r) }
getReturnVars (Val (EnvV i)) r = r { escapedEnvs = Set.insert i (escapedEnvs r) }
getReturnVars (Not x) m = getReturnVars x m
getReturnVars (Fst _ x) m = getReturnVars x m
getReturnVars (Snd _ x) m = getReturnVars x m
getReturnVars (IsEmpty _ x) m = getReturnVars x m
getReturnVars (HeadList _ x) m = getReturnVars x m
getReturnVars (TailList _ x) m = getReturnVars x m
getReturnVars (CastExpr _ x) m = getReturnVars x m
getReturnVars (Box _ x) m = getReturnVars x m
getReturnVars (Unbox _ x) m = getReturnVars x m
getReturnVars (Ternary _ _ t e) m = getReturnVars t (getReturnVars e m)
getReturnVars (ConsList _ x y) m = getReturnVars x (getReturnVars y m)
getReturnVars (Prod _ _ x y) m = getReturnVars x (getReturnVars y m)
getReturnVars (LIntOp _ x y) m = getReturnVars x (getReturnVars y m)
getReturnVars (LCmpOp _ x y) m = getReturnVars x (getReturnVars y m)
getReturnVars (CallExpr _ _ f x) m = getReturnVars f (getReturnVars x m)
getReturnVars (ApplyClosure _ f x) m = getReturnVars f (getReturnVars x m)
getReturnVars (IndexList _ x y) m = getReturnVars x (getReturnVars y m)
getReturnVars _ m = m

escapeAnalysis :: CStatement a -> EscapeResult -> EscapeResult
escapeAnalysis (DefFun _ _ _ body) r = escapeAnalysis body r
escapeAnalysis (Seq x y) r = escapeAnalysis y (escapeAnalysis x r)
escapeAnalysis (Return x) r = getReturnVars x r
escapeAnalysis (BindExpr _ x i y) r = escapeAnalysis y (countVarUses x (r { varUses = Map.insertWith (+) i 1 (varUses r) }))  -- x doesn't escape by being bound
escapeAnalysis (If c t e) r =
    let r' = countVarUses c r
    in mergeEscape (escapeAnalysis t r') (escapeAnalysis e r')
escapeAnalysis (UpdateVar _ i x) r = countVarUses x (r { varUses = Map.insertWith (+) i 1 (varUses r), varDefs = Map.insert i (CArg CTVoidPtr x) (varDefs r)})
escapeAnalysis (DefVar _ i x) r = countVarUses x (r { varUses = Map.insert i 0 (varUses r), varDefs = Map.insert i (CArg CTVoidPtr x) (varDefs r) })
escapeAnalysis (While c x) r = escapeAnalysis x (countVarUses c r)
escapeAnalysis (AllocEnv i _ _ _) r = r { allocedEnvs = Set.insert i (allocedEnvs r) }
escapeAnalysis _ r = r

removeLocalEnvsExpr :: CExpression a -> EscapeResult -> CExpression a
removeLocalEnvsExpr (GetEnvField t envId varId) r =
    if Set.member envId (escapedEnvs r)
    then GetEnvField t envId varId
    else if Set.member envId (allocedEnvs r) then -- can only remove closure if it was actually alloced in this function, ontherwise it was passed as a param
            -- trace ("MEMBER: " ++ show envId ++ " | " ++  show r) $
            Var t varId
        else GetEnvField t envId varId
removeLocalEnvsExpr (Not x) r = Not (removeLocalEnvsExpr x r)
removeLocalEnvsExpr (Fst t x) r = Fst t (removeLocalEnvsExpr x r)
removeLocalEnvsExpr (Snd t x) r = Snd t (removeLocalEnvsExpr x r)
removeLocalEnvsExpr (IsEmpty t x) r = IsEmpty t (removeLocalEnvsExpr x r)
removeLocalEnvsExpr (HeadList t x) r = HeadList t (removeLocalEnvsExpr x r)
removeLocalEnvsExpr (TailList t x) r = TailList t (removeLocalEnvsExpr x r)
removeLocalEnvsExpr (Box t x) r = Box t (removeLocalEnvsExpr x r)
removeLocalEnvsExpr (Unbox t x) r = Unbox t (removeLocalEnvsExpr x r)
removeLocalEnvsExpr (CastExpr t x) r = CastExpr t (removeLocalEnvsExpr x r)
removeLocalEnvsExpr (LIntOp op x y) r = LIntOp op (removeLocalEnvsExpr x r) (removeLocalEnvsExpr y r)
removeLocalEnvsExpr (LCmpOp op x y) r = LCmpOp op (removeLocalEnvsExpr x r) (removeLocalEnvsExpr y r)
removeLocalEnvsExpr (Ternary t c x y) r = Ternary t (removeLocalEnvsExpr c r) (removeLocalEnvsExpr x r) (removeLocalEnvsExpr y r)
removeLocalEnvsExpr (CallExpr tf tx f x) r = CallExpr tf tx (removeLocalEnvsExpr f r) (removeLocalEnvsExpr x r)
removeLocalEnvsExpr (ApplyClosure t f x) r = ApplyClosure t (removeLocalEnvsExpr f r) (removeLocalEnvsExpr x r)
removeLocalEnvsExpr (ConsList t x y) r = ConsList t (removeLocalEnvsExpr x r) (removeLocalEnvsExpr y r)
removeLocalEnvsExpr (Prod tx ty x y) r = Prod tx ty (removeLocalEnvsExpr x r) (removeLocalEnvsExpr y r)
removeLocalEnvsExpr (IndexList t x y) r = IndexList t (removeLocalEnvsExpr x r) (removeLocalEnvsExpr y r)
removeLocalEnvsExpr x _ = x

removeLocalEnvs :: CStatement a -> EscapeResult -> CStatement a
removeLocalEnvs (AllocEnv envId parentId directPs parentPs) r =
    if Set.member envId (escapedEnvs r) then
        if Set.member parentId (escapedEnvs r) then
            AllocEnv envId parentId directPs parentPs
        else -- if parent got removed erge parentsPs into directPs
            AllocEnv envId parentId (directPs ++ parentPs) []
    else Skip
removeLocalEnvs (DefFun t i p body) _ =
    let r' = escapeAnalysis body (EscapeResult Set.empty Map.empty Map.empty Set.empty Set.empty)
    in DefFun t i p (removeLocalEnvs body r')
removeLocalEnvs (Seq x y) r = Seq (removeLocalEnvs x r) (removeLocalEnvs y r)
removeLocalEnvs (BindExpr t x i y) r = BindExpr t (removeLocalEnvsExpr x r) i (removeLocalEnvs y r)
removeLocalEnvs (If c x y) r = If (removeLocalEnvsExpr c r) (removeLocalEnvs x r) (removeLocalEnvs y r)
removeLocalEnvs (While c x) r = While (removeLocalEnvsExpr c r) (removeLocalEnvs x r)
removeLocalEnvs (DefVar t i x) r = DefVar t i (removeLocalEnvsExpr x r)
removeLocalEnvs (UpdateVar t i x) r = UpdateVar t i (removeLocalEnvsExpr x r)
removeLocalEnvs (Return x) r = Return (removeLocalEnvsExpr x r)
removeLocalEnvs x _ = x

removeSingleVarsExpr :: CExpression a -> EscapeResult -> CExpression a
removeSingleVarsExpr (Var t i) r =
    case Map.lookup i (varUses r) of
        Just n | n <= 1 ->
            case Map.lookup i (varDefs r) of
                Just (CArg _ x) -> removeSingleVarsExpr (unsafeCoerce x) r
                _ -> Var t i
        _ -> Var t i
removeSingleVarsExpr (Not x) r = Not (removeSingleVarsExpr x r)
removeSingleVarsExpr (Fst t x) r = Fst t (removeSingleVarsExpr x r)
removeSingleVarsExpr (Snd t x) r = Snd t (removeSingleVarsExpr x r)
removeSingleVarsExpr (IsEmpty t x) r = IsEmpty t (removeSingleVarsExpr x r)
removeSingleVarsExpr (HeadList t x) r = HeadList t (removeSingleVarsExpr x r)
removeSingleVarsExpr (TailList t x) r = TailList t (removeSingleVarsExpr x r)
removeSingleVarsExpr (Box t x) r = Box t (removeSingleVarsExpr x r)
removeSingleVarsExpr (Unbox t x) r = Unbox t (removeSingleVarsExpr x r)
removeSingleVarsExpr (CastExpr t x) r = CastExpr t (removeSingleVarsExpr x r)
removeSingleVarsExpr (LIntOp op x y) r = LIntOp op (removeSingleVarsExpr x r) (removeSingleVarsExpr y r)
removeSingleVarsExpr (LCmpOp op x y) r = LCmpOp op (removeSingleVarsExpr x r) (removeSingleVarsExpr y r)
removeSingleVarsExpr (Ternary t c x y) r = Ternary t (removeSingleVarsExpr c r) (removeSingleVarsExpr x r) (removeSingleVarsExpr y r)
removeSingleVarsExpr (CallExpr tf tx f x) r = CallExpr tf tx (removeSingleVarsExpr f r) (removeSingleVarsExpr x r)
removeSingleVarsExpr (ApplyClosure t f x) r = ApplyClosure t (removeSingleVarsExpr f r) (removeSingleVarsExpr x r)
removeSingleVarsExpr (ConsList t x y) r = ConsList t (removeSingleVarsExpr x r) (removeSingleVarsExpr y r)
removeSingleVarsExpr (Prod tx ty x y) r = Prod tx ty (removeSingleVarsExpr x r) (removeSingleVarsExpr y r)
removeSingleVarsExpr (IndexList t x y) r = IndexList t (removeSingleVarsExpr x r) (removeSingleVarsExpr y r)
removeSingleVarsExpr x _ = x

removeSingleVars :: CStatement a -> EscapeResult -> CStatement a
removeSingleVars (DefFun t i p body) _ =
    let r' = escapeAnalysis body (EscapeResult Set.empty Map.empty Map.empty Set.empty Set.empty)
    in DefFun t i p (removeSingleVars body r')
removeSingleVars (Seq x y) r = Seq (removeSingleVars x r) (removeSingleVars y r)
removeSingleVars (BindExpr t x i y) r =
    case Map.lookup i (varUses r) of
        Just n | n <= 1 -> removeSingleVars y r
        _ -> BindExpr t (removeSingleVarsExpr x r) i (removeSingleVars y r)
removeSingleVars (If c x y) r = If (removeSingleVarsExpr c r) (removeSingleVars x r) (removeSingleVars y r)
removeSingleVars (While c x) r = While (removeSingleVarsExpr c r) (removeSingleVars x r)
removeSingleVars (DefVar t i x) r =
    case Map.lookup i (varUses r) of
        Just n | n <= 1 -> Skip
        _ -> DefVar t i (removeSingleVarsExpr x r)
removeSingleVars (UpdateVar t i x) r =
    case Map.lookup i (varUses r) of
        Just n | n <= 1 -> Skip
        _ -> UpdateVar t i (removeSingleVarsExpr x r)
removeSingleVars (Return x) r = Return (removeSingleVarsExpr x r)
removeSingleVars x _ = x

countUsedEnvs :: CStatement a -> Set.Set Int -> Set.Set Int
countUsedEnvs (AllocEnv _ i _ _) m = Set.insert i m
countUsedEnvs (Seq x y) m = countUsedEnvs y (countUsedEnvs x m)
countUsedEnvs (If c x y) m = countUsedEnvs y (countUsedEnvs x (countUsedEnvsExpr c m))
countUsedEnvs (While c x) m = countUsedEnvs x (countUsedEnvsExpr c m)
countUsedEnvs (DefFun _ _ params body) m =
    let m' = foldr (\p acc -> case p of
                CParamEnv i -> Set.insert i acc
                _ -> acc) m params
    in countUsedEnvs body m'
countUsedEnvs (BindExpr _ x _ y) m = countUsedEnvs y (countUsedEnvsExpr x m)
countUsedEnvs (Return x) m = countUsedEnvsExpr x m
countUsedEnvs (DefVar _ _ x) m = countUsedEnvsExpr x m
countUsedEnvs (UpdateVar _ _ x) m = countUsedEnvsExpr x m
countUsedEnvs _ m = m

countUsedEnvsExpr :: CExpression a -> Set.Set Int -> Set.Set Int
countUsedEnvsExpr (Val (EnvV i)) m = Set.insert i m
countUsedEnvsExpr (GetEnvField _ i _) m = Set.insert i m
countUsedEnvsExpr (CallExpr _ _ f x) m = countUsedEnvsExpr x (countUsedEnvsExpr f m)
countUsedEnvsExpr (ApplyClosure _ f x) m = countUsedEnvsExpr x (countUsedEnvsExpr f m)
countUsedEnvsExpr (Ternary _ c t e) m = countUsedEnvsExpr e (countUsedEnvsExpr t (countUsedEnvsExpr c m))
countUsedEnvsExpr (LIntOp _ x y) m = countUsedEnvsExpr y (countUsedEnvsExpr x m)
countUsedEnvsExpr (LCmpOp _ x y) m = countUsedEnvsExpr y (countUsedEnvsExpr x m)
countUsedEnvsExpr (ConsList _ x y) m = countUsedEnvsExpr y (countUsedEnvsExpr x m)
countUsedEnvsExpr (Prod _ _ x y) m = countUsedEnvsExpr y (countUsedEnvsExpr x m)
countUsedEnvsExpr (Fst _ x) m = countUsedEnvsExpr x m
countUsedEnvsExpr (Snd _ x) m = countUsedEnvsExpr x m
countUsedEnvsExpr (Not x) m = countUsedEnvsExpr x m
countUsedEnvsExpr (IsEmpty _ x) m = countUsedEnvsExpr x m
countUsedEnvsExpr (HeadList _ x) m = countUsedEnvsExpr x m
countUsedEnvsExpr (TailList _ x) m = countUsedEnvsExpr x m
countUsedEnvsExpr (Box _ x) m = countUsedEnvsExpr x m
countUsedEnvsExpr (Unbox _ x) m = countUsedEnvsExpr x m
countUsedEnvsExpr (CastExpr _ x) m = countUsedEnvsExpr x m
countUsedEnvsExpr (IndexList _ x y) m = countUsedEnvsExpr y (countUsedEnvsExpr x m)
countUsedEnvsExpr _ m = m



-- REMOVE CLOSURE ALLOCS
countClosureUses :: Int -> CStatement a -> Int
countClosureUses i (Return x) = countClosureUsesExpr i x
countClosureUses i (Seq x y) = countClosureUses i x + countClosureUses i y
countClosureUses i (BindExpr _ x _ y) = countClosureUsesExpr i x + countClosureUses i y
countClosureUses i (If c t e) = countClosureUsesExpr i c + countClosureUses i t + countClosureUses i e
countClosureUses i (While c x) = countClosureUsesExpr i c + countClosureUses i x
countClosureUses _ _ = 0

countClosureUsesExpr :: Int -> CExpression a -> Int
countClosureUsesExpr i (Val (ClosureV j)) = if i == j then 1 else 0
countClosureUsesExpr i (ApplyClosure _ f x) = countClosureUsesExpr i f + countClosureUsesExpr i x
countClosureUsesExpr i expr@CallExpr{} =
    let (func, args) = collectArgs expr
        funcCount = countClosureUsesExpr i func
        argsCount = foldr (\(CArg _ a) acc -> countClosureUsesExpr i a + acc) 0 args
    in funcCount + argsCount
countClosureUsesExpr i (Ternary _ c t e) = countClosureUsesExpr i c + countClosureUsesExpr i t + countClosureUsesExpr i e
countClosureUsesExpr i (CastExpr _ f) = countClosureUsesExpr i f
countClosureUsesExpr i (Box _ f) = countClosureUsesExpr i f
countClosureUsesExpr i (Unbox _ f) = countClosureUsesExpr i f
countClosureUsesExpr _ _ = 0

-- closure id, 
rewriteApply :: Int -> Int -> CExpression a -> CExpression a
rewriteApply i parentId (ApplyClosure targ f arg) =
    case f of
        Val (ClosureV i') | i == i' ->
            -- trace ("single arg case " ++ showCExpression f Map.empty) $
            CallExpr CTVoidPtr targ (CallExpr CTVoidPtr CTVoidPtr (Var CTVoidPtr i) (Val (EnvV parentId))) (stripBox arg)
        _ ->
            -- trace ("multi arg case " ++ showCExpression f Map.empty) $
            case rewriteApply i parentId f of
                expr@(CallExpr tf _ _ _) -> CallExpr tf targ (unsafeCoerce expr) arg
                f' -> ApplyClosure targ f' arg
rewriteApply _ _ x = x

-- i is the id of the closureAlloc were getting rid of
    -- if we find the application of that closure we need to rewrite it to a callexpr
rewriteClosureUseExpr :: Int -> Int -> CExpression b -> CExpression b
rewriteClosureUseExpr i parentId (ApplyClosure targ f arg) = rewriteApply i parentId (ApplyClosure targ f arg)
rewriteClosureUseExpr i parentId (Ternary tp c t e) =
    Ternary tp (rewriteClosureUseExpr i parentId c) (rewriteClosureUseExpr i parentId t) (rewriteClosureUseExpr i parentId e)
rewriteClosureUseExpr i parentId (CallExpr tf tx f x) = CallExpr tf tx (rewriteClosureUseExpr i parentId f) (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr i parentId (CastExpr t x) = CastExpr t (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr i parentId (Box t x) = Box t (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr i parentId (Unbox t x) = Unbox t (rewriteClosureUseExpr i parentId x)
rewriteClosureUseExpr _ _ x = x

-- rewrite the single use of closure i to a direct call using envVar
rewriteClosureUse :: Int -> Int -> CStatement b -> CStatement b
rewriteClosureUse i parentId (Return x) = Return (rewriteClosureUseExpr i parentId x)
rewriteClosureUse i parentId (Seq (AllocClosure j) y) | i == j = rewriteClosureUse i parentId y
rewriteClosureUse i parentId (Seq (AllocEnv j _ [] _) y) | i == j = rewriteClosureUse i parentId y
rewriteClosureUse i parentId (Seq x y) = Seq (rewriteClosureUse i parentId x) (rewriteClosureUse i parentId y)
rewriteClosureUse i parentId (BindExpr t x j y) = BindExpr t (rewriteClosureUseExpr i parentId x) j (rewriteClosureUse i parentId y)
rewriteClosureUse i parentId (If c t e) = If (rewriteClosureUseExpr i parentId c) (rewriteClosureUse i parentId t) (rewriteClosureUse i parentId e)
rewriteClosureUse i parentId (While c x) = While (rewriteClosureUseExpr i parentId c) (rewriteClosureUse i parentId x)
rewriteClosureUse _ _ x = x

-- top level pass, if we alloc a closure that is only ever used once afterward we can get rid of it
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




-- INLINING

countFunctionCallsExpr :: CExpression a -> Map.Map Int Int -> Map.Map Int Int
countFunctionCallsExpr (CallExpr tf tx f x) m =
    let (func, args) = collectArgs (CallExpr tf tx f x)
        m' = case func of
                Var _ i -> Map.insertWith (+) i 1 m
                _ -> m
    in foldr (\(CArg _ a) acc -> countFunctionCallsExpr a acc) m' args
countFunctionCallsExpr (Not x) m = countFunctionCallsExpr x m
countFunctionCallsExpr (LIntOp _ x y) m = countFunctionCallsExpr y (countFunctionCallsExpr x m)
countFunctionCallsExpr (LCmpOp _ x y) m = countFunctionCallsExpr y (countFunctionCallsExpr x m)
countFunctionCallsExpr (Ternary _ x y z) m = countFunctionCallsExpr z (countFunctionCallsExpr y (countFunctionCallsExpr x m))
countFunctionCallsExpr (Prod _ _ x y) m = countFunctionCallsExpr y (countFunctionCallsExpr x m)
countFunctionCallsExpr (Fst _ x) m = countFunctionCallsExpr x m
countFunctionCallsExpr (Snd _ x) m = countFunctionCallsExpr x m
countFunctionCallsExpr (IsEmpty _ x) m = countFunctionCallsExpr x m
countFunctionCallsExpr (HeadList _ x) m = countFunctionCallsExpr x m
countFunctionCallsExpr (TailList _ x) m = countFunctionCallsExpr x m
countFunctionCallsExpr (IndexList _ x y) m = countFunctionCallsExpr y (countFunctionCallsExpr x m)
countFunctionCallsExpr (ConsList _ x y) m = countFunctionCallsExpr y (countFunctionCallsExpr x m)
countFunctionCallsExpr (ApplyClosure _ x y) m = countFunctionCallsExpr y (countFunctionCallsExpr x m)
countFunctionCallsExpr (CastExpr _ y) m = countFunctionCallsExpr y m
countFunctionCallsExpr (Box _ y) m = countFunctionCallsExpr y m
countFunctionCallsExpr (Unbox _ y) m = countFunctionCallsExpr y m
countFunctionCallsExpr _ m = m

countFunctionCalls :: CStatement a -> Map.Map Int Int -> Map.Map Int Int
countFunctionCalls (DefFun _ _ _ body) m = countFunctionCalls body m
countFunctionCalls (Return x) m = countFunctionCallsExpr x m
countFunctionCalls (DefVar _ _ x) m = countFunctionCallsExpr x m
countFunctionCalls (UpdateVar _ _ x) m = countFunctionCallsExpr x m
countFunctionCalls (Seq x y) m = countFunctionCalls y (countFunctionCalls x m)
countFunctionCalls (If c x y) m =
    let m' = countFunctionCallsExpr c m
        mx = countFunctionCalls x Map.empty
        my = countFunctionCalls y Map.empty
    in Map.unionWith (+) m' (Map.unionWith max mx my)
countFunctionCalls (BindExpr _ x _ y) m = countFunctionCalls y (countFunctionCallsExpr x m)
countFunctionCalls (While c x) m = countFunctionCalls x (countFunctionCallsExpr c m)
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
                Just DefFun {} -> True
                _ -> False) callMap
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
replaceReturn _ _ x = x

-- get a default value to init the new var before the if statement
defaultVal :: CType -> CExpression a
defaultVal CTInt  = unsafeCoerce (Val (IntV 0))
defaultVal CTBool = unsafeCoerce (Val (BoolV False))
defaultVal _      = unsafeCoerce (Val UnitV)

-- Inline all calls to function i throughout body
inlineOne :: Int -> [CStatement a] -> CStatement a -> (CStatement a, Bool)
inlineOne i defs body =
    case getFun i defs of
        Just (DefFun tret ifun params fbody) ->
            if endsInIf fbody then (
                let retExpr = Var tret ifun
                    bodyNoRet = Seq (DefVar tret ifun (defaultVal tret)) (replaceReturn ifun tret fbody)
                    body' = inlineCallsTo i params bodyNoRet retExpr body
                in (body', True))
            else (
                let retExpr = findFirstReturn fbody
                    bodyNoRet = removeFirstReturn fbody
                    body' = inlineCallsTo i params bodyNoRet retExpr body
                in (body', True))
        _ -> (body, False)

-- Replace all CallExpr (Var i) args with inlined body
-- Replace ternary with if so that we can add the pre work
inlineCallsTo :: Int -> CParams -> CStatement a -> CExpression a -> CStatement a -> CStatement a
inlineCallsTo i params fbodyNoRet retExpr = goStmt
  where
    goStmt (Return x) =
        case x of
            Ternary _ c t e ->
                let (pt, t') = goExpr t
                    (pe, e') = goExpr e
                    (pc, c') = goExpr c
                in Seq pc $ If c' (Seq pt (Return t')) (Seq pe (Return e'))
            _ ->
                let (pre, x') = goExpr x
                in Seq pre (Return x')
    goStmt (Seq x y) = Seq (goStmt x) (goStmt y)
    goStmt (BindExpr t x j y) = let (pre, x') = goExpr x
                                in Seq pre (BindExpr t x' j (goStmt y))
    goStmt (If c x y) = let (pre, c') = goExpr c
                        in Seq pre (If c' (goStmt x) (goStmt y))
    goStmt (DefFun t j ps b) = DefFun t j ps (goStmt b)
    goStmt (While c x) = let (pre, c') = goExpr c
                                 in Seq pre (While c' (goStmt x))
    goStmt (DefVar t j x) = let (pre, x') = goExpr x
                             in Seq pre (DefVar t j x')
    goStmt (UpdateVar t j x) = let (pre, x') = goExpr x
                                in Seq pre (UpdateVar t j x')
    goStmt x = x

    goExpr :: CExpression b -> (CStatement a, CExpression b)
    goExpr expr@(CallExpr tf _ _ _) =
        let (func, args) = collectArgs expr
        in
            -- trace (if i == 75 then "Hello " ++ showCExpression func Map.empty else "") $
            case func of
            Var _ j | j == i ->
                let
                    funArgs = take (length params) args
                    bindings = foldr
                        (\pair acc -> case pair of
                            (CParam ip tp, CArg _ arg) ->
                                Seq (DefVar tp ip arg) acc
                            (CParamEnv ip, CArg _ arg) ->
                                case arg of
                                    (Val (EnvV ip'))
                                        | ip' == ip -> acc
                                        | otherwise -> Seq (DefVar CTVoidPtr ip arg) acc
                                    _ -> error "mismatch arg and param"
                        ) Skip (zip params funArgs)
                    pre = Seq bindings fbodyNoRet
                in (unsafeCoerce pre, unsafeCoerce retExpr)
            -- _ -> (Skip, expr)
            _ ->
                let argResults = map (\(CArg t a) -> let (p, a') = goExpr a in (p, CArg t a')) args
                    totalPre = foldr (\(p, _) acc -> Seq p acc) Skip argResults
                    args' = map snd argResults
                    (funcPre, func') = goExpr func
                in (Seq funcPre totalPre, rebuildCall tf func' args')
    goExpr (Ternary tp c t e) =
        let (pc, c') = goExpr c
            (pt, t') = goExpr t
            (pe, e') = goExpr e
        in (Seq pc (Seq pt pe), Ternary tp c' t' e')
    goExpr (Not x) = let (p, x') = goExpr x in (p, Not x')
    goExpr (IsEmpty t x) = let (p, x') = goExpr x in (p, IsEmpty t x')
    goExpr (HeadList t x) = let (p, x') = goExpr x in (p, HeadList t x')
    goExpr (TailList t x) = let (p, x') = goExpr x in (p, TailList t x')
    goExpr (ConsList t x y) =
        let (px, x') = goExpr x
            (py, y') = goExpr y
        in (Seq px py, ConsList t x' y')
    goExpr (LIntOp op x y) =    let (px, x') = goExpr x
                                    (py, y') = goExpr y
                            in (Seq px py, LIntOp op x' y')
    goExpr (LCmpOp op x y) =    let (px, x') = goExpr x
                                    (py, y') = goExpr y
                            in (Seq px py, LCmpOp op x' y')
    goExpr (Fst t x) = let (p, x') = goExpr x in (p, Fst t x')
    goExpr (Snd t x) = let (p, x') = goExpr x in (p, Snd t x')
    goExpr (CastExpr t x)= let (p, x') = goExpr x in (p, CastExpr t x')
    goExpr (ApplyClosure tx f x) =
        let (pf, f') = goExpr f
            (px, x') = goExpr x
        in (Seq pf px, ApplyClosure tx f' x')
    goExpr (Box t x) = let (p, x') = goExpr x
                        in (p, Box t x')
    goExpr (Unbox t x) = let (p, x') = goExpr x in (p, Unbox t x')
    goExpr x = (Skip, x)

-- Keep inlining until nothing changes
inlineUntilFixed :: [CStatement a] -> CStatement a -> (CStatement a, [Int])
inlineUntilFixed defs body =
    let (body', removed) = inlinePass defs body
    in
        -- trace ("until fixed " ++ show (countFunctionCalls body' Map.empty)) $
        if null removed
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



-- REMOVE CASTS
removeCast :: CExpression a -> CExpression b -> CExpression a
removeCast fallback x = case x of
    Val cv           -> unsafeCoerce (Val cv)
    Not ce           -> unsafeCoerce (Not ce)
    Var ct n         -> unsafeCoerce (Var ct n)
    LIntOp op a b    -> unsafeCoerce (LIntOp op a b)
    LCmpOp op a b    -> unsafeCoerce (LCmpOp op a b)
    expr@Prod{}      -> unsafeCoerce expr
    expr@HeadList{}  -> unsafeCoerce expr
    expr@TailList{}  -> unsafeCoerce expr
    expr@EmptyList{} -> unsafeCoerce expr
    expr@ConsList{}  -> unsafeCoerce expr
    expr@IsEmpty{}   -> unsafeCoerce expr
    expr@IndexList{} -> unsafeCoerce expr
    expr@CallExpr{}  -> unsafeCoerce expr
    _                -> fallback -- the only ones that need cast are fst and snd because they return void*

removeCastsExpr :: CExpression a -> CExpression a
removeCastsExpr (CastExpr t x) = removeCast (CastExpr t x) (removeCastsExpr x)
removeCastsExpr (Unbox t x)    = removeCast (Unbox t x)    (removeCastsExpr x)
removeCastsExpr (Box t x)      = Box t (removeCastsExpr x)
removeCastsExpr (Not x)        = Not (removeCastsExpr x)
removeCastsExpr (LIntOp op x y)    = LIntOp op (removeCastsExpr x) (removeCastsExpr y)
removeCastsExpr (LCmpOp op x y)    = LCmpOp op (removeCastsExpr x) (removeCastsExpr y)
removeCastsExpr (Ternary t c x y)  = Ternary t (removeCastsExpr c) (removeCastsExpr x) (removeCastsExpr y)
removeCastsExpr (CallExpr tf tx f x) = CallExpr tf tx (removeCastsExpr f) (removeCastsExpr x)
removeCastsExpr (ApplyClosure t f x) = ApplyClosure t (removeCastsExpr f) (removeCastsExpr x)
removeCastsExpr (ConsList t x y)   = ConsList t (removeCastsExpr x) (removeCastsExpr y)
removeCastsExpr (Prod tx ty x y)   = Prod tx ty (removeCastsExpr x) (removeCastsExpr y)
removeCastsExpr (Fst t x)          = Fst t (removeCastsExpr x)
removeCastsExpr (Snd t x)          = Snd t (removeCastsExpr x)
removeCastsExpr (IsEmpty t x)      = IsEmpty t (removeCastsExpr x)
removeCastsExpr (HeadList t x)     = HeadList t (removeCastsExpr x)
removeCastsExpr (TailList t x)     = TailList t (removeCastsExpr x)
removeCastsExpr (IndexList t x y)  = IndexList t (removeCastsExpr x) (removeCastsExpr y)
removeCastsExpr x                  = x

removeCasts :: CStatement a -> CStatement a
removeCasts (Return x)          = Return (removeCastsExpr x)
removeCasts (Seq x y)           = Seq (removeCasts x) (removeCasts y)
removeCasts (BindExpr t x i y)  = BindExpr t (removeCastsExpr x) i (removeCasts y)
removeCasts (If c x y)          = If (removeCastsExpr c) (removeCasts x) (removeCasts y)
removeCasts (While c x)         = While (removeCastsExpr c) (removeCasts x)
removeCasts (DefFun t i ps x)   = DefFun t i ps (removeCasts x)
removeCasts (DefVar t i x)      = DefVar t i (removeCastsExpr x)
removeCasts (UpdateVar t i x)   = UpdateVar t i (removeCastsExpr x)
removeCasts x                   = x


-- REMOVE USELESS LOGIC (i.e. making a list out of the head and tail of another list)

-- compare cexpression just by string
eqCExpr :: CExpression a -> CExpression b -> Bool
eqCExpr x y = showCExpression x Map.empty == showCExpression y Map.empty

removeUselessExpr :: CExpression a -> CExpression a
removeUselessExpr (ConsList t x y) =
    let x' = stripWrap x
        y' = stripWrap y
    in case (x', y') of
        (HeadList _ l1, TailList _ l2) | eqCExpr l1 l2 -> unsafeCoerce l1
        _ -> ConsList t (removeUselessExpr x) (removeUselessExpr y)
    where
        stripWrap (Unbox _ r) = unsafeCoerce r
        stripWrap (CastExpr _ r) = unsafeCoerce r
        stripWrap (Box _ r) = unsafeCoerce r
        stripWrap r = r
removeUselessExpr (LIntOp op x y) = LIntOp op (removeUselessExpr x) (removeUselessExpr y)
removeUselessExpr (LCmpOp op x y) = LCmpOp op (removeUselessExpr x) (removeUselessExpr y)
removeUselessExpr (Ternary t c x y) = Ternary t (removeUselessExpr c) (removeUselessExpr x) (removeUselessExpr y)
removeUselessExpr (CallExpr tf tx f x) = CallExpr tf tx (removeUselessExpr f) (removeUselessExpr x)
removeUselessExpr (ApplyClosure t f x) = ApplyClosure t (removeUselessExpr f) (removeUselessExpr x)
removeUselessExpr (Prod tx ty x y) = Prod tx ty (removeUselessExpr x) (removeUselessExpr y)
removeUselessExpr (Fst t x) = Fst t (removeUselessExpr x)
removeUselessExpr (Snd t x) = Snd t (removeUselessExpr x)
removeUselessExpr (IsEmpty t x) = IsEmpty t (removeUselessExpr x)
removeUselessExpr (HeadList t x) = HeadList t (removeUselessExpr x)
removeUselessExpr (TailList t x) = TailList t (removeUselessExpr x)
removeUselessExpr (IndexList t x y) = IndexList t (removeUselessExpr x) (removeUselessExpr y)
removeUselessExpr (Not x) = Not (removeUselessExpr x)
removeUselessExpr (Box t x) = Box t (removeUselessExpr x)
removeUselessExpr (Unbox t x) = Unbox t (removeUselessExpr x)
removeUselessExpr (CastExpr t x) = CastExpr t (removeUselessExpr x)
removeUselessExpr x = x

removeUseless :: CStatement a -> CStatement a
removeUseless (Return x) = Return (removeUselessExpr x)
removeUseless (Seq x y) = Seq (removeUseless x) (removeUseless y)
removeUseless (BindExpr t x i y) = BindExpr t (removeUselessExpr x) i (removeUseless y)
removeUseless (If c x y) = If (removeUselessExpr c) (removeUseless x) (removeUseless y)
removeUseless (While c x) = While (removeUselessExpr c) (removeUseless x)
removeUseless (DefFun t i ps x) = DefFun t i ps (removeUseless x)
removeUseless (DefVar t i x) = DefVar t i (removeUselessExpr x)
removeUseless (UpdateVar t i x) = UpdateVar t i (removeUselessExpr x)
removeUseless x = x



-- Function unrolling

-- int fact_iter(int n, int acc) {
--     while (1) {
--         if (n <= 1) return acc;
--         acc = acc * n;
--         n = n - 1;
--         // loop instead of recursive call
--     }
-- }

isTailRecursive :: Int -> CStatement a -> Bool
isTailRecursive ifun body =
    trace ("RETURN " ++ show ifun ++ " ||| " ++ case findReturn body of
        Just n -> showCExpression n Map.empty
        Nothing -> "nothing") $
    case findReturn body of
        Just (Ternary _ _ t e) -> hasTailCall ifun t || hasTailCall ifun e
        Just expr -> hasTailCall ifun expr
        Nothing   -> False

hasTailCall :: Int -> CExpression a -> Bool
hasTailCall ifun (CallExpr _ _ f _) = outermostVar f == Just ifun
hasTailCall ifun (Ternary _ _ t e) = hasTailCall ifun t || hasTailCall ifun e
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

printIntArgMap :: Map.Map Int CArg -> String
printIntArgMap m = intercalate ", " $ map (\(i, arg) -> "v" ++ show i ++ " -> " ++ showCArg arg Map.empty) (Map.toList m)

-- Common Subexpression elimination

collectAliases :: CStatement a -> Map.Map Int CArg -> Map.Map Int CArg
collectAliases (DefVar t i x) m =
    case x of
        Var t2 j -> Map.insert i (CArg t (Var t2 j)) m
        Val (EnvV j) -> Map.insert i (CArg t (Val (EnvV j))) m
        _ -> m
collectAliases (Seq x y) m = collectAliases y (collectAliases x m)
collectAliases (If _ x y) m = Map.union (collectAliases x m) (collectAliases y m)
collectAliases (While _ x) m = collectAliases x m
collectAliases (BindExpr _ _ _ y) m = collectAliases y m
collectAliases (DefFun _ _ _ body) m = collectAliases body m
collectAliases _ m = m

applyAliases :: CStatement a -> Map.Map Int CArg -> CStatement a
applyAliases (DefVar t i x) m =
    case Map.lookup i m of
        Just _ -> Skip
        Nothing -> DefVar t i (replaceVarAssignment x m)
applyAliases (BindExpr t x i y) m = BindExpr t (replaceVarAssignment x m) i (applyAliases y m)
applyAliases (Seq x y) m = Seq (applyAliases x m) (applyAliases y m)
applyAliases (If cond x y) m = If (replaceVarAssignment cond m) (applyAliases x m) (applyAliases y m)
applyAliases (While cond x) m = While (replaceVarAssignment cond m) (applyAliases x m)
applyAliases (DefFun tret ifun param body) m = DefFun tret ifun param (applyAliases body m)
applyAliases (Return x) m = Return (replaceVarAssignment x m)
applyAliases (UpdateVar tx i x) m = UpdateVar tx i (replaceVarAssignment x m)
applyAliases x _ = x

-- returns bool true if something changed
eliminateAliases :: CStatement a -> (CStatement a, Bool)
eliminateAliases stmt =
    let m = collectAliases stmt Map.empty
    in trace (printIntArgMap m) $
        (applyAliases stmt m, not (null m))

replaceVarAssignment :: CExpression a -> Map.Map Int CArg -> CExpression a
replaceVarAssignment (Var t i) m =
    case Map.lookup i m of
        Just (CArg _ n) -> unsafeCoerce n
        Nothing -> Var t i
replaceVarAssignment (GetEnvField t structId fieldId) m =
    -- trace ("!!!!replacing getenv " ++ show structId) $
    case Map.lookup structId m of
        Just (CArg _ (Var _ newId)) -> GetEnvField t newId fieldId
        Just (CArg _ (Val (EnvV newId))) -> GetEnvField t newId fieldId
        _ -> GetEnvField t structId fieldId
replaceVarAssignment (Not x) m = Not (replaceVarAssignment x m)
replaceVarAssignment (LIntOp op x y) m = LIntOp op (replaceVarAssignment x m) (replaceVarAssignment y m)
replaceVarAssignment (LCmpOp op x y) m = LCmpOp op (replaceVarAssignment x m) (replaceVarAssignment y m)
replaceVarAssignment (Ternary tp x y z) m = Ternary tp (replaceVarAssignment x m) (replaceVarAssignment y m) (replaceVarAssignment z m)
replaceVarAssignment (CallExpr tf tx x y) m = CallExpr tf tx (replaceVarAssignment x m) (replaceVarAssignment y m)
replaceVarAssignment (Prod tx ty x y) m = Prod tx ty (replaceVarAssignment x m) (replaceVarAssignment y m)
replaceVarAssignment (Fst t x) m = Fst t (replaceVarAssignment x m)
replaceVarAssignment (Snd t x) m = Snd t (replaceVarAssignment x m)
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

-- Not used
collapseBoxExpr :: CExpression a -> CExpression a
collapseBoxExpr (Box t (Box _ x))     = Box t (collapseBoxExpr x)
collapseBoxExpr (Unbox t (Unbox _ x)) = Unbox t (collapseBoxExpr x)
collapseBoxExpr (Unbox _ (Box _ x))   = unsafeCoerce collapseBoxExpr x
collapseBoxExpr (Box t x)             = Box t (collapseBoxExpr x)
collapseBoxExpr (Unbox t x)           = Unbox t (collapseBoxExpr x)
collapseBoxExpr (CallExpr tf tx f x) = CallExpr tf tx (collapseBoxExpr f) (collapseBoxExpr x)
collapseBoxExpr (ApplyClosure t f x)  = ApplyClosure t (collapseBoxExpr f) (collapseBoxExpr x)
collapseBoxExpr (Ternary t c x y)     = Ternary t (collapseBoxExpr c) (collapseBoxExpr x) (collapseBoxExpr y)
collapseBoxExpr (LIntOp op x y)       = LIntOp op (collapseBoxExpr x) (collapseBoxExpr y)
collapseBoxExpr (LCmpOp op x y)       = LCmpOp op (collapseBoxExpr x) (collapseBoxExpr y)
collapseBoxExpr (Not x)               = Not (collapseBoxExpr x)
collapseBoxExpr (ConsList t x y)      = ConsList t (collapseBoxExpr x) (collapseBoxExpr y)
collapseBoxExpr (Prod tx ty x y)      = Prod tx ty (collapseBoxExpr x) (collapseBoxExpr y)
collapseBoxExpr (Fst t x)             = Fst t (collapseBoxExpr x)
collapseBoxExpr (Snd t x)             = Snd t (collapseBoxExpr x)
collapseBoxExpr (IsEmpty t x)          = IsEmpty t (collapseBoxExpr x)
collapseBoxExpr (HeadList t x)          = HeadList t (collapseBoxExpr x)
collapseBoxExpr (TailList t x)          = TailList t (collapseBoxExpr x)
collapseBoxExpr (CastExpr t x)        = CastExpr t (collapseBoxExpr x)
collapseBoxExpr (IndexList t x y)     = IndexList t (collapseBoxExpr x) (collapseBoxExpr y)
collapseBoxExpr x                     = x

-- Not used
collapseBox :: CStatement a -> CStatement a
collapseBox (Return x) = Return (collapseBoxExpr x)
collapseBox (Seq x y) = Seq (collapseBox x) (collapseBox y)
collapseBox (BindExpr t x i y) = BindExpr t (collapseBoxExpr x) i (collapseBox y)
collapseBox (If c x y) = If (collapseBoxExpr c) (collapseBox x) (collapseBox y)
collapseBox (While c x) = While (collapseBoxExpr c) (collapseBox x)
collapseBox (DefFun t i ps x) = DefFun t i ps (collapseBox x)
collapseBox (DefVar t i x) = DefVar t i (collapseBoxExpr x)
collapseBox (UpdateVar t i x) = UpdateVar t i (collapseBoxExpr x)
collapseBox x = x


printEscapeAnalysis :: [CStatement a] -> IO ()
printEscapeAnalysis = mapM_ printOne
  where
    printOne def@(DefFun _ i _ _) = do
        let result = escapeAnalysis def (EscapeResult Set.empty Map.empty Map.empty Set.empty Set.empty)
        putStrLn $ "v" ++ show i ++ ": " ++ show result
    printOne _ = return ()

getDefs :: CStatement a -> [CStatement a]
getDefs stmt@DefFun{} = [stmt]
getDefs (Seq x y) = getDefs x ++ getDefs y
getDefs _ = []

-- MAIN

keepOptimising :: CStatement a -> [CStatement a] -> Map.Map Int Int -> (CStatement a, [CStatement a], Map.Map Int Int)
keepOptimising body defs mergedMap = do
    let (cbody', removedClosures) = removeClosureAllocs body
    let mergedMap' = foldr (\i m -> Map.insert i (Map.findWithDefault 1 i m + 1) m) mergedMap removedClosures
    let defs' = map (fst . removeClosureAllocs) defs
    let changedClosures = not (null removedClosures)

    let (inlinedBody, changedFuns) =
        -- trace ("inlined " ++ show (countFunctionCalls cbody' Map.empty)) $
            let (cbody'', removedFuns) = inlineUntilFixed defs' cbody'
                (cbody''', _) = removeDeadFuns removedFuns defs' cbody''
            in (cbody''', not (null removedFuns))

    let (varReplacedBody, changedAlias) = eliminateAliases inlinedBody

    let escapeRes = map (`escapeAnalysis` emptyEscapeResult) (getDefs varReplacedBody)
    let escapeBody = removeLocalEnvs varReplacedBody (foldr mergeEscape emptyEscapeResult escapeRes)
    let escapeBody' = removeSingleVars escapeBody (foldr mergeEscape emptyEscapeResult escapeRes)

    let removedUselessBody = removeUseless escapeBody'
    let newDefs = getDefs removedUselessBody

    let changed = changedClosures || changedAlias || changedFuns
    if not changed
        then (removedUselessBody, newDefs, mergedMap')
        else keepOptimising removedUselessBody newDefs mergedMap'

helloRun :: Typeable a => String -> AL.Lang a -> IO ()
helloRun progName progCode = do
    -- let libName = "\n#include \"" ++ "../"  ++ "listLib.c\"\n"
    -- let progPath = 
    --         if canInline then "inlined/" ++ progName
    --         else "removedClosureAllocs/" ++ progName
    let libName = "\n#include \"listLib.c\"\n"
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
    print mergedMap

    putStrLn "\n--- Lifting Lambdas ---"
    let (cbody0, closureEnv, liftenv, _, defs0) = lambdaLift merged
    putStrLn $ showCStmt 0 Map.empty Map.empty Map.empty cbody0
    let cbody = addBoxing cbody0 -- boxing values
    let defs = map addBoxing defs0
    let strFunTypes = getStrFunTypes defs Map.empty
    let (finalBody', finalDefs, finalMergeMap) = keepOptimising cbody defs mergedMap
    let finalBody = removeCasts finalBody'

    putStrLn "\n--- Printing C ---"
    let imports =   "\n#include <stdbool.h>" ++
                    "\n#include <stdio.h>" ++
                    "\n#include <stdlib.h>" ++
                    "\n#include <stdint.h>" ++
                    libName

    -- printEscapeAnalysis finalDefs

    let usedEnvs = countUsedEnvs finalBody Set.empty
    let closureDefs = filter (\d -> case d of
            DefFun _ i _ _ -> Set.member i usedEnvs
            _ -> False) finalDefs

    let closureStructs = generateClosureStructs closureDefs liftenv
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

    mapM_ (uncurry helloRun) progsInt
    mapM_ (uncurry helloRun) progsList