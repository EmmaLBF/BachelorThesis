{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module DeadCodePass where
import C
import CDefs
import AST
import Utils
import DemotePass
import qualified Data.Map as Map
import qualified Data.Set as Set


-- (1) REMOVE ENV PARAM FROM FUNCTIONS 
-- collects list of functions that had their envs removed
-- removes first env params for functions where it is not used
envRemovalPass :: CStatement -> CStatement
envRemovalPass body =
    let l = Set.fromList
            [ ifun | fun@(DefFun _ ifun params _) <- getDefs body
                , let usedEnvs = envUses (getFunctionInfo fun emptyFunctionInfo)
                , any (\p -> case p of CParamEnv i | i == ifun -> not (Set.member i usedEnvs); _ -> False) params]
    in demoteEnvs (removeUselessEnvParam body (-1) l)

-- (1b) remove collected envs
removeUselessEnvParam :: CStatement -> Int -> RemovedEnvs -> CStatement
removeUselessEnvParam (DefFun t ifun params body) _ s = 
    let p' = [p | p <- params, case p of CParamEnv i -> i /= ifun || ifun `notElem` s; _ -> True]
    in DefFun t ifun p' (removeUselessEnvParam body ifun s)
removeUselessEnvParam s ifun m = mapChildrenStmt (\x -> removeUselessEnvParam x ifun m) (\e -> removeUselessEnvParamExpr e ifun m) s

-- (1c) drop the env parameter from the call expressions that are affected
-- remove env access to just var access
removeUselessEnvParamExpr :: CExpression -> Int -> RemovedEnvs -> CExpression
removeUselessEnvParamExpr (GetEnvField t envId varId) ifun s
    | envId == ifun && ifun `elem` s = Var t varId
removeUselessEnvParamExpr (CallExpr tf tx f x) ifun s =
    let (f', args) = collectArgs (CallExpr tf tx f x)
        fId = case f' of Var _ i -> i; _ -> -1
    in  if fId `elem` s 
        then let args' = case args of (CArg _ (Val (EnvV _)) : rest) -> rest; _ -> args
             in rebuildCall tf f' (map (\(CArg t arg) -> CArg t (removeUselessEnvParamExpr arg ifun s)) args')
        else CallExpr tf tx (removeUselessEnvParamExpr f ifun s) (removeUselessEnvParamExpr x ifun s)
removeUselessEnvParamExpr e ifun s = mapChildrenExpr (\x -> removeUselessEnvParamExpr x ifun s) e


-- (2) REMOVING ALIASES
eliminateAliases :: CStatement -> CStatement
eliminateAliases stmt =
    let info = getGlobalInfo stmt emptyGlobalInfo
        stmt' = replaceAliases stmt (aliasesGlobal info)
    in  if stmt == stmt' then stmt' else eliminateAliases stmt'

-- (2a) replaces all var defs of form v2 = v3 or v2 = env5
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


-- (3) REMOVE VARS USED <= 1 TIMES
removeSingleVars :: CStatement -> CStatement -> FunctionInfo -> CStatement
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

usedAtMostOnce :: Int -> VarUses -> Bool
usedAtMostOnce i m = case Map.lookup i m of
    Just n | n <= 1 -> True
    _ -> False


--  (4) REMOVE USLESS LOGIC + CASTS (i.e. making a list out of the head and tail of another list)
-- cannot remove cast from things that return erased type
removeCast :: CExpression -> CExpression -> CExpression
removeCast fallback x = case x of
    ApplyClosure{} -> fallback
    CallExpr{} -> fallback
    expr -> expr

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

-- (5) CLEAN IR (REMOVE SKIPS)
-- cleanSkip :: CStatement -> CStatement
-- cleanSkip (Seq Skip y) = cleanSkip y
-- cleanSkip (Seq y Skip) = cleanSkip y
-- cleanSkip s = mapChildrenStmt cleanSkip id s
-- cleanSkip s =
--     case mapChildrenStmt cleanSkip id s of
--         Seq Skip y -> y
--         Seq y Skip -> y
--         s'         -> s

cleanSkip :: CStatement -> CStatement
cleanSkip (Seq x y) =
    case (cleanSkip x, cleanSkip y) of
        (Skip, y') -> y'
        (x', Skip) -> x'
        (x', y')   -> Seq x' y'
cleanSkip s = mapChildrenStmt cleanSkip id s
