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


-- ***** REMOVE ENV PARAM FROM FUNCTIONS ***** 

removeCallParamEnv :: CStatement -> Int -> Set.Set Int -> CStatement
removeCallParamEnv (DefFun t i p body) _ s = DefFun t i p (removeCallParamEnv body i s)
removeCallParamEnv s ifun m = mapChildrenStmt (\x -> removeCallParamEnv x ifun m) (\e -> removeCallParamEnvExpr e ifun m) s

removeCallParamEnvExpr :: CExpression -> Int -> Set.Set Int -> CExpression
removeCallParamEnvExpr (GetEnvField t envId varId) ifun s
    | envId == ifun && ifun `elem` s = Var t varId
removeCallParamEnvExpr (CallExpr tf tx f x) ifun s =
    let (f', args) = collectArgs (CallExpr tf tx f x)
        fId = case f' of Var _ i -> i; _ -> -1
    in  if fId `elem` s
        then
            let args' = case args of (CArg _ (Val (EnvV _)) : rest) -> rest; _ -> args
            in rebuildCall tf f' (map (\(CArg t arg) -> CArg t (removeCallParamEnvExpr arg ifun s)) args')
        else CallExpr tf tx (removeCallParamEnvExpr f ifun s) (removeCallParamEnvExpr x ifun s)
removeCallParamEnvExpr e ifun s = mapChildrenExpr (\x -> removeCallParamEnvExpr x ifun s) e

-- removes first env params for functions where it is not used
removeEnvParam :: CStatement -> CStatement -> FunctionInfo -> State (Set.Set Int) CStatement
removeEnvParam (DefFun t ifun params body) stmt _ =
    let r' = getFunctionInfo (DefFun t ifun params body) emptyFunctionInfo
        p' = [p | p <- params, case p of CParamEnv i -> Set.member i (envUses r'); _ -> True]
    in do
        when (params /= p') $ modify (Set.insert ifun)
        body' <- removeEnvParam body stmt r'
        return $ DefFun t ifun p' body'
removeEnvParam (Seq x y) stmt r = Seq <$> removeEnvParam x stmt r <*> removeEnvParam y stmt r
removeEnvParam x _ _ = return x

envRemovalPass :: CStatement -> CStatement
envRemovalPass body =
    let (removedEnvParamBody, l) = runState (removeEnvParam body body emptyFunctionInfo) Set.empty
        paramRemoved = removeCallParamEnv removedEnvParamBody (-1) l
    in removeEnvs paramRemoved

-- ***** REMOVE LOCAL ENVS ***** 

-- removes envs that are only used locally (no alloc needed)
removeEnvs :: CStatement -> CStatement
removeEnvs (DefFun t i p body) =
    let removed = collectUnusedEnvAllocs body emptyFunctionInfo
    in DefFun t i p (rewriteRemovedEnvs removed body)
removeEnvs (Seq x y) = Seq (removeEnvs x) (removeEnvs y)
removeEnvs x = x

-- PASS 1: remove the AllocEnv statements that aren't needed, collect their ids
    -- can only be removed if it does not escape anywhere
collectUnusedEnvAllocs :: CStatement -> FunctionInfo -> Set.Set Int
collectUnusedEnvAllocs (AllocEnv envId _ _ _) r | envId `notElem` escapedEnvs r = Set.singleton envId
collectUnusedEnvAllocs s@(DefFun _ _ _ body) _ =
    collectUnusedEnvAllocs body (getFunctionInfo s emptyFunctionInfo)
collectUnusedEnvAllocs s r =
    foldr Set.union Set.empty ([collectUnusedEnvAllocs c r | c <- childrenStmt s])

-- PASS 2: rewrites get-env-field accesses to the envs that were removed, and removes alloc statements
rewriteRemovedEnvs :: Set.Set Int -> CStatement -> CStatement
rewriteRemovedEnvs r (AllocEnv envId _ _ _) | envId `elem` r = Skip
rewriteRemovedEnvs r s = mapChildrenStmt (rewriteRemovedEnvs r) (rewriteRemovedEnvsExpr r) s

rewriteRemovedEnvsExpr :: Set.Set Int -> CExpression -> CExpression
rewriteRemovedEnvsExpr removed (GetEnvField t envId varId)
    | Set.member envId removed = rewriteRemovedEnvsExpr removed (Var t varId)
    | otherwise = GetEnvField t envId varId
rewriteRemovedEnvsExpr removed e = mapChildrenExpr (rewriteRemovedEnvsExpr removed) e


-- ***** REMOVE VARS (THAT ARE USED <= 1 TIMES) ***** 

usedAtMostOnce :: Int -> Map.Map Int Int -> Bool
usedAtMostOnce i m = case Map.lookup i m of
    Just n | n <= 1 -> True
    _ -> False

removeSingleVars :: CStatement -> CStatement -> FunctionInfo -> CStatement
removeSingleVars (BindExpr _ _ i y) stmt r | usedAtMostOnce i (varUses r) = removeSingleVars y stmt r
removeSingleVars (DefVar _ i _) _ r | usedAtMostOnce i (varUses r) = Skip
removeSingleVars (UpdateVar _ i _) _ r | usedAtMostOnce i (varUses r)  = Skip
removeSingleVars s@(DefFun t i p body) stmt _ =
    DefFun t i p (removeSingleVars body stmt (getFunctionInfo s emptyFunctionInfo))
removeSingleVars s stmt r =
    mapChildrenStmt (\x -> removeSingleVars x stmt r) (\e -> removeSingleVarsExpr e stmt r) s

removeSingleVarsExpr :: CExpression -> CStatement -> FunctionInfo -> CExpression
removeSingleVarsExpr (Var t i) stmt r | usedAtMostOnce i (varUses r) =
    case Map.lookup i (varDefs r) of
        (Just (CArg _ x)) -> removeSingleVarsExpr x stmt r
        _ -> Var t i
removeSingleVarsExpr (Val (ClosureV i)) stmt r | usedAtMostOnce i (varUses r) =
    case Map.lookup i (varDefs r) of
        Just (CArg _ x) -> removeSingleVarsExpr x stmt r
        _ -> Val (ClosureV i)
removeSingleVarsExpr e stmt r = mapChildrenExpr (\x -> removeSingleVarsExpr x stmt r) e


-- ***** REMOVE CLOSURES ***** 




-- CLEAN IR (REMOVE SKIPS)

cleanSkip :: CStatement -> CStatement
cleanSkip (Seq Skip y) = cleanSkip y
cleanSkip (Seq y Skip) = cleanSkip y
cleanSkip s = mapChildrenStmt cleanSkip id s

-- the only ones that need cast are fst and snd because they return void*
removeCast :: CExpression -> CExpression -> CExpression
removeCast fallback x = case x of
    Fst{} -> fallback
    Snd{} -> fallback
    expr -> expr

-- REMOVE USELESS LOGIC AND CASTS (i.e. making a list out of the head and tail of another list)
removeUselessExpr :: CExpression -> CExpression
removeUselessExpr (CastExpr t x) = removeCast (CastExpr t x) (removeUselessExpr x)
removeUselessExpr (Unbox t x) = removeCast (Unbox t x) (removeUselessExpr x)
removeUselessExpr (ConsList t x y) =
    case (stripWrap x, stripWrap y) of
        (HeadList _ l1, TailList _ l2) | l1 == l2 -> l1
        _ -> ConsList t (removeUselessExpr x) (removeUselessExpr y)
removeUselessExpr e = mapChildrenExpr removeUselessExpr e

removeUselessStmt :: CStatement -> CStatement
removeUselessStmt = mapChildrenStmt removeUselessStmt removeUselessExpr

-- REMOVING ALIASES

eliminateAliases :: CStatement -> CStatement
eliminateAliases stmt =
    let info = getGlobalInfo stmt emptyGlobalInfo
        stmt' = replaceAliases stmt (aliases info)
    in  if stmt == stmt'
        then stmt'
        else eliminateAliases stmt'

-- replaces all var defs of form v2 = v3 or v2 = env5
replaceAliases :: CStatement -> Map.Map Int CArg -> CStatement
replaceAliases s m = mapChildrenStmt (`replaceAliases` m) (`replaceAliasesExpr` m) s

replaceAliasesExpr :: CExpression -> CArgMap -> CExpression
replaceAliasesExpr (Var t i) m =
    case Map.lookup i m of
        Just (CArg _ n) -> replaceAliasesExpr n m
        Nothing -> Var t i
replaceAliasesExpr (GetEnvField t structId fieldId) m =
    case Map.lookup structId m of
        Just (CArg _ (Var _ newId)) -> replaceAliasesExpr (GetEnvField t newId fieldId) m
        Just (CArg _ (Val (EnvV newId))) -> replaceAliasesExpr (GetEnvField t newId fieldId) m
        _ -> GetEnvField t structId fieldId
replaceAliasesExpr e m = mapChildrenExpr (`replaceAliasesExpr` m) e
