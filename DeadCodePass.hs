{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DeadCodePass where
import C
import CDefs
import AST
import Utils
import DemotePass
import qualified Data.Map as Map
import qualified Data.Set as Set


-- ***** REMOVE ENV PARAM FROM FUNCTIONS ***** 

-- (1) Overall Pass
envRemovalPass :: CStatement -> CStatement
envRemovalPass body =
    let l = collectUselessEnvParam body emptyFunctionInfo
        paramRemoved = removeUselessEnvParam body (-1) l
    in demoteEnvs paramRemoved

-- (2) removes first env params for functions where it is not used
-- collects list of functions that had their envs removed
collectUselessEnvParam :: CStatement -> FunctionInfo -> RemovedEnvs
collectUselessEnvParam (DefFun t ifun params body) _ =
    let r' = getFunctionInfo (DefFun t ifun params body) emptyFunctionInfo
        p' = [p | p <- params, case p of CParamEnv i -> Set.member i (envUses r'); _ -> True]
    in if params /= p' then Set.singleton ifun else Set.empty
collectUselessEnvParam (Seq x y) r = Set.union (collectUselessEnvParam x r) (collectUselessEnvParam y r)
collectUselessEnvParam _ _ = Set.empty

-- (3a) remove collected envs
removeUselessEnvParam :: CStatement -> Int -> RemovedEnvs -> CStatement
removeUselessEnvParam (DefFun t ifun params body) _ s = 
    let p' = [p | p <- params, case p of CParamEnv i -> i /= ifun || ifun `notElem` s; _ -> True]
    in DefFun t ifun p' (removeUselessEnvParam body ifun s)
removeUselessEnvParam s ifun m = mapChildrenStmt (\x -> removeUselessEnvParam x ifun m) (\e -> removeUselessEnvParamExpr e ifun m) s

-- (3b) drop the env parameter from the call expressions that are affected
-- remove env access to just var access
removeUselessEnvParamExpr :: CExpression -> Int -> RemovedEnvs -> CExpression
removeUselessEnvParamExpr (GetEnvField t envId varId) ifun s
    | envId == ifun && ifun `elem` s = Var t varId
removeUselessEnvParamExpr (CallExpr tf tx f x) ifun s =
    let (f', args) = collectArgs (CallExpr tf tx f x)
        fId = case f' of Var _ i -> i; _ -> -1
    in  if fId `elem` s
        then
            let args' = case args of (CArg _ (Val (EnvV _)) : rest) -> rest; _ -> args
            in rebuildCall tf f' (map (\(CArg t arg) -> CArg t (removeUselessEnvParamExpr arg ifun s)) args')
        else CallExpr tf tx (removeUselessEnvParamExpr f ifun s) (removeUselessEnvParamExpr x ifun s)
removeUselessEnvParamExpr e ifun s = mapChildrenExpr (\x -> removeUselessEnvParamExpr x ifun s) e


-- ***** REMOVE VARS (THAT ARE USED <= 1 TIMES) ***** 

-- helper
usedAtMostOnce :: Int -> VarUses -> Bool
usedAtMostOnce i m = case Map.lookup i m of
    Just n | n <= 1 -> True
    _ -> False

-- (1a)
removeSingleVars :: CStatement -> CStatement -> FunctionInfo -> CStatement
removeSingleVars (DefVar _ i _) _ r | usedAtMostOnce i (varUses r) = Skip
removeSingleVars (UpdateVar _ i _) _ r | usedAtMostOnce i (varUses r)  = Skip
removeSingleVars s@(DefFun t i p body) stmt _ =
    DefFun t i p (removeSingleVars body stmt (getFunctionInfo s emptyFunctionInfo))
removeSingleVars s stmt r =
    mapChildrenStmt (\x -> removeSingleVars x stmt r) (\e -> removeSingleVarsExpr e stmt r) s

-- (1b)
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


-- CLEAN IR (REMOVE SKIPS)

cleanSkip :: CStatement -> CStatement
cleanSkip (Seq Skip y) = cleanSkip y
cleanSkip (Seq y Skip) = cleanSkip y
cleanSkip s = mapChildrenStmt cleanSkip id s

-- REMOVE USLESS + CASTS

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
    in  if stmt == stmt' then stmt' else eliminateAliases stmt'

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
