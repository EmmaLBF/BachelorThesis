{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DeadCodePass where
import C
import CDefs
import AST
import Utils
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Unsafe.Coerce (unsafeCoerce)


-- ***** REMOVE ENV PARAM FROM FUNCTIONS ***** 

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
        then Var t varId
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
removeUnusedParams :: CParams -> FunctionInfo -> CParams
removeUnusedParams [] _ = []
removeUnusedParams [CParam i t] _ = [CParam i t]
removeUnusedParams [CParamEnv i] m = if i `elem` envUses m then [CParamEnv i] else []
removeUnusedParams (i:is) m = removeUnusedParams [i] m ++ removeUnusedParams is m

-- removes first env params for functions where it is not used
removeEnvParam :: CStatement a -> CStatement b -> FunctionInfo -> State (Set.Set Int) (CStatement a)
removeEnvParam (DefFun t i p body) stmt _ =
    let r' = getFunctionInfo (DefFun t i p body) emptyFunctionInfo
        p' = removeUnusedParams p r'
    in do
        when (p /= p') $ modify (Set.insert i)
        body' <- removeEnvParam body stmt r'
        return $ DefFun t i p' body'
removeEnvParam (Seq x y) stmt r = Seq <$> removeEnvParam x stmt r <*> removeEnvParam y stmt r
removeEnvParam x _ _ = return x

envParamRemovalPass :: CStatement a -> CStatement a
envParamRemovalPass body =
    let (removedEnvParamBody, l) = runState (removeEnvParam body body emptyFunctionInfo) Set.empty
    in removeCallParamEnv removedEnvParamBody (-1) l


-- ***** REMOVE LOCAL ENVS ***** 
-- removes envs that are only used locally (no alloc needed)

-- PASS 2: rewrites get-env-field accesses to the envs that were removed
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
    | Set.member envId removed = rewriteRemovedEnvsExpr removed (Var t varId)
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
        (body', removed) = runState (removeEnvAllocs body (DefFun t i p body) r') Set.empty
    in DefFun t i p (rewriteRemovedEnvs removed body')
removeEnvs (Seq x y) = Seq (removeEnvs x) (removeEnvs y)
removeEnvs x = x

-- PASS 1: remove the AllocEnv statements that aren't needed, collect their ids
    -- can only be removed if it does not escape anywhere
removeEnvAllocs :: CStatement a -> CStatement b -> FunctionInfo -> State (Set.Set Int) (CStatement a)
removeEnvAllocs (AllocEnv envId parentId directPs parentPs) _ r =
    if Set.member envId (escapedEnvs r)
    then return $ AllocEnv envId parentId directPs parentPs
    else do
        modify (Set.insert envId)
        return Skip
removeEnvAllocs (BindExpr t x i y) stmt r = BindExpr t x i <$> removeEnvAllocs y stmt r
removeEnvAllocs (DefVar t i x) _ _ = return $ DefVar t i x
removeEnvAllocs (UpdateVar t i x) _ _ = return $ UpdateVar t i x
removeEnvAllocs (DefFun t i p body) stmt _ = do
    let r' = getFunctionInfo (DefFun t i p body) emptyFunctionInfo
    DefFun t i p <$> removeEnvAllocs body stmt r'
removeEnvAllocs (Seq x y) stmt r = Seq <$> removeEnvAllocs x stmt r <*> removeEnvAllocs y stmt r
removeEnvAllocs (If c x y) stmt r = If c <$> removeEnvAllocs x stmt r <*> removeEnvAllocs y stmt r
removeEnvAllocs (While c x) stmt r = While c <$> removeEnvAllocs x stmt r
removeEnvAllocs (Return x) _ _ = return $ Return x
removeEnvAllocs x _ _ = return x



-- *****  REMOVE VARS (THAT ARE USED <= 1 TIMES)

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


-- ***** REMOVE CLOSURES ***** 

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
removeClosureAllocs :: CStatement a -> FunctionInfo -> State [(Int, Int)] (CStatement a)
removeClosureAllocs (AllocClosure i) g
    | i `elem` escapedClos g = return $ AllocClosure i
    | otherwise = do
        modify ((i, i) :)
        return Skip
removeClosureAllocs (Seq x y) g = Seq <$> removeClosureAllocs x g <*> removeClosureAllocs y g
removeClosureAllocs (If cond x y) g = If cond <$> removeClosureAllocs x g <*> removeClosureAllocs y g
removeClosureAllocs (While cond x) g = While cond <$> removeClosureAllocs x g 
removeClosureAllocs (DefFun tret ifun ps x) _ =
    let (x', r) = runState (removeClosureAllocs x (getFunctionInfo x emptyFunctionInfo)) []
        x'' = foldr (uncurry rewriteClosureUse) x' r
    in return $ DefFun tret ifun ps x''
removeClosureAllocs (BindExpr t x i y) g = BindExpr t x i <$> removeClosureAllocs y g 
removeClosureAllocs x _ = return x





-- ****** INLINE FUNCTIONS


-- CLEAN IR (REMOVE SKIPS)

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
    case (stripWrap x, stripWrap y) of
        (HeadList _ l1, TailList _ l2) | l1 == l2 -> l1
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



-- REMOVING ALIASES AND LOCAL ENVS

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

eliminateAliases :: CStatement a -> CStatement a
eliminateAliases stmt =
    let info = getGlobalInfo stmt emptyGlobalInfo
        stmt' = removeEnvs (applyAliases stmt (aliases info))
    in  if stmt == stmt'
        then stmt'
        else eliminateAliases stmt'

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
