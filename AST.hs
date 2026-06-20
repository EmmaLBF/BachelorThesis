{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AST where

import CDefs
import qualified Data.Set as Set
import qualified Data.Map as Map
import Utils (childrenExpr)
import Data.List (foldl')

-- Global AST info traversal

addPairType :: CType -> GlobalInfo -> GlobalInfo
addPairType t m =
    case t of
        CTPtr (CTPair tx ty) -> m {pairTypesGlobal = Set.insert (tx, ty) (pairTypesGlobal m)}
        CTPair tx ty -> m {pairTypesGlobal = Set.insert (tx, ty) (pairTypesGlobal m)}
        _ -> m

addEnvUse :: Int -> FunctionInfo -> FunctionInfo
addEnvUse i f = f { envUses = Set.insert i (envUses f) }
addEnvEscape :: Int -> FunctionInfo -> FunctionInfo
addEnvEscape i f = f { escapedEnvs = Set.insert i (escapedEnvs f) }
addEnvAlloc :: Int -> FunctionInfo -> FunctionInfo
addEnvAlloc i f = f { allocedEnvs = Set.insert i (allocedEnvs f) }
addClosEscape :: Int -> FunctionInfo -> FunctionInfo
addClosEscape i f = f { escapedClos = Set.insert i (escapedClos f) }
addVarEscape :: Int -> FunctionInfo -> FunctionInfo
addVarEscape i f = f { escapedVars = Set.insert i (escapedVars f) }
addVarUse :: Int -> FunctionInfo -> FunctionInfo
addVarUse i f = f { varUses =  Map.insertWith (+) i 1 (varUses f) }

addAlias :: CExpression -> Int -> CType -> GlobalInfo -> GlobalInfo
addAlias x i t m =
    case x of
        Val (EnvV j) -> m { aliasesGlobal = Map.insert i (CArg t (Val (EnvV j))) (aliasesGlobal m)}
        Val (ClosureV j) -> m { aliasesGlobal = Map.insert i (CArg t (Val (ClosureV j))) (aliasesGlobal m)}
        HeadList t2 var@Var{} -> m { aliasesGlobal = Map.insert i (CArg t (HeadList t2 var)) (aliasesGlobal m)}
        TailList t2 var@Var{} -> m { aliasesGlobal = Map.insert i (CArg t (TailList t2 var)) (aliasesGlobal m)}
        expr@Var{} -> m { aliasesGlobal = Map.insert i (CArg t expr) (aliasesGlobal m)}
        expr@Fst{} -> m { aliasesGlobal = Map.insert i (CArg t expr) (aliasesGlobal m)}
        expr@Snd{} -> m { aliasesGlobal = Map.insert i (CArg t expr) (aliasesGlobal m)}
        _ -> m

emptyGlobalInfo :: GlobalInfo
emptyGlobalInfo = GlobalInfo Set.empty Map.empty Map.empty Set.empty

mergeGlobalInfo :: GlobalInfo -> GlobalInfo -> GlobalInfo
mergeGlobalInfo a b = GlobalInfo
    (Set.union (usedEnvsGlobal a) (usedEnvsGlobal b))
    (Map.unionWith (+) (funCallsGlobal a) (funCallsGlobal b))
    (Map.union (aliasesGlobal a) (aliasesGlobal b))
    (Set.union (pairTypesGlobal a) (pairTypesGlobal b))

getGlobalInfo :: CStatement -> GlobalInfo -> GlobalInfo
getGlobalInfo (AllocEnv _ i params ) m =
    let m' = m { usedEnvsGlobal = Set.insert i (usedEnvsGlobal m) }
    in foldr (\(CArg _ x) acc -> getGlobalInfoExpr x acc) m' params
getGlobalInfo (DefFun t _ params body) m =
    let m' = foldr (\p acc ->
                case p of
                    CParamEnv i -> m { usedEnvsGlobal = Set.insert i (usedEnvsGlobal m) }
                    _ -> acc ) m params
        m'' = addPairType t m'
    in getGlobalInfo body m''
getGlobalInfo (DefVar t i x) m = addPairType t (getGlobalInfoExpr x (addAlias x i t m))
getGlobalInfo (UpdateVar t _ x) m = addPairType t (getGlobalInfoExpr x m)
getGlobalInfo (Seq x y) m = getGlobalInfo y (getGlobalInfo x m)
getGlobalInfo (If c x y) m = getGlobalInfo y (getGlobalInfo x (getGlobalInfoExpr c m))
getGlobalInfo (While c x) m = getGlobalInfo x (getGlobalInfoExpr c m)
getGlobalInfo (Return x) m = getGlobalInfoExpr x m
getGlobalInfo _ m = m

getGlobalInfoExpr :: CExpression -> GlobalInfo -> GlobalInfo
getGlobalInfoExpr (Val (EnvV i)) m = m { usedEnvsGlobal = Set.insert i (usedEnvsGlobal m)}
getGlobalInfoExpr (GetEnvField t i _) m = addPairType t (m { usedEnvsGlobal = Set.insert i (usedEnvsGlobal m)})
getGlobalInfoExpr (CallExpr tf tx f x) m =
    let (func, args) = CDefs.collectArgs (CallExpr tf tx f x)
        m' = case func of
                Var _ i -> m { funCallsGlobal = Map.insertWith (+) i 1 (funCallsGlobal m)}
                _ -> getGlobalInfoExpr f m
        m'' = addPairType tx (addPairType tf m')
    in foldr (\(CArg _ a) acc -> getGlobalInfoExpr a acc) m'' args
getGlobalInfoExpr (ApplyClosure t f x) m = addPairType t (getGlobalInfoExpr x (getGlobalInfoExpr f m))
getGlobalInfoExpr (Ternary tp c t e) m = addPairType tp (getGlobalInfoExpr e (getGlobalInfoExpr t (getGlobalInfoExpr c m)))
getGlobalInfoExpr (ConsList t x y) m = addPairType t (getGlobalInfoExpr y (getGlobalInfoExpr x m))
getGlobalInfoExpr (Prod t x y) m = addPairType t (getGlobalInfoExpr y (getGlobalInfoExpr x m))
getGlobalInfoExpr (Fst t t2 x) m = addPairType t2 (addPairType t (getGlobalInfoExpr x m))
getGlobalInfoExpr (Snd t t2 x) m = addPairType t2 (addPairType t (getGlobalInfoExpr x m))
getGlobalInfoExpr (IsEmpty t x) m = addPairType t (getGlobalInfoExpr x m)
getGlobalInfoExpr (HeadList t x) m = addPairType t (getGlobalInfoExpr x m)
getGlobalInfoExpr (TailList t x) m = addPairType t (getGlobalInfoExpr x m)
getGlobalInfoExpr (Box t x) m = addPairType t (getGlobalInfoExpr x m)
getGlobalInfoExpr (Unbox t x) m = addPairType t (getGlobalInfoExpr x m)
getGlobalInfoExpr (CastExpr t x) m = addPairType t (getGlobalInfoExpr x m)
getGlobalInfoExpr (IndexList t x y) m = addPairType t (getGlobalInfoExpr y (getGlobalInfoExpr x m))
getGlobalInfoExpr (LIntOp _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr (LCmpOp _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr (LBoolOp _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr (Not x) m = getGlobalInfoExpr x m
getGlobalInfoExpr (Abs x) m = getGlobalInfoExpr x m
getGlobalInfoExpr _ m = m

-- Per function body AST info traversal

emptyFunctionInfo :: FunctionInfo
emptyFunctionInfo = FunctionInfo Set.empty Map.empty Map.empty Set.empty Set.empty Set.empty Map.empty Set.empty Set.empty

mergeFunctionInfo :: FunctionInfo -> FunctionInfo -> FunctionInfo
mergeFunctionInfo a b = FunctionInfo
    (Set.union (escapedVars a) (escapedVars b))
    (Map.unionWith max (varUses a) (varUses b))
    (Map.union (varDefs a) (varDefs b))
    (Set.union (escapedEnvs a) (escapedEnvs b))
    (Set.union (allocedEnvs a) (allocedEnvs b))
    (Set.union (envUses a) (envUses b))
    (Map.unionWith (+) (functionCalls a) (functionCalls b))
    (Set.union (escapedClos a) (escapedClos b))
    (Set.union (allocedClos a) (allocedClos b))

getFunctionInfoExpr :: Bool -> CExpression -> FunctionInfo -> FunctionInfo
getFunctionInfoExpr _ (GetEnvField _ envId _) r = addEnvUse envId r
getFunctionInfoExpr escapes (Var _ i) r =
    if escapes then addVarEscape i (addVarUse i r) else addVarUse i r
getFunctionInfoExpr escapes (Val (EnvV i)) r =
    if escapes then addEnvEscape i (addEnvUse i r) else addEnvUse i r
getFunctionInfoExpr escapes (Val (ClosureV i)) r =
    if escapes then addClosEscape i (addVarUse i r) else addVarUse i r
getFunctionInfoExpr escapes (CallExpr tf tx f x) m =
    let (func, args) = CDefs.collectArgs (CallExpr tf tx f x)
        m' = case func of
                Var _ i -> m { functionCalls = Map.insertWith (+) i 1 (functionCalls m) }
                _ -> getFunctionInfoExpr escapes f m
    in foldr (\(CArg _ a) acc -> case a of Val (EnvV _) -> acc; _ -> getFunctionInfoExpr True a acc) m' args
getFunctionInfoExpr escapes (ApplyClosure _ f x) m = getFunctionInfoExpr False f (getFunctionInfoExpr escapes x m)
getFunctionInfoExpr escape e m = foldl' (flip (getFunctionInfoExpr escape)) m (childrenExpr e)

getFunctionInfo :: CStatement -> FunctionInfo -> FunctionInfo
getFunctionInfo (DefFun _ _ _ body) r = getFunctionInfo body r
getFunctionInfo (Seq x y) r = getFunctionInfo y (getFunctionInfo x r)
getFunctionInfo (Return x) r = getFunctionInfoExpr True x r
getFunctionInfo (If c t e) r =
    let r' = getFunctionInfoExpr False c r
    in mergeFunctionInfo (getFunctionInfo t r') (getFunctionInfo e r')
getFunctionInfo (UpdateVar t i x) r = addVarUse i (getFunctionInfoExpr False x (r {varDefs = Map.insert i (CArg t x) (varDefs r)}))
getFunctionInfo (DefVar t i x) r = getFunctionInfoExpr False x (r { varUses = Map.insert i 0 (varUses r), varDefs = Map.insert i (CArg t x) (varDefs r) })
getFunctionInfo (While c x) r = getFunctionInfo x (getFunctionInfoExpr False c r)
getFunctionInfo (AllocEnv i _ params) r =
    foldr (\(CArg _ x) acc -> getFunctionInfoExpr False x acc) (addEnvUse i (addEnvAlloc i r)) params
getFunctionInfo (AllocClosure i) r = addEnvEscape i (addEnvUse i (r {allocedClos = Set.insert i (allocedClos r)}))
getFunctionInfo _ r = r
