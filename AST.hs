{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AST where

import CDefs
import qualified Data.Set as Set
import qualified Data.Map as Map
import Utils (childrenExpr)
import Data.List (foldl')

-- Global AST info traversal

addPairType :: CType -> Set.Set (CType, CType) -> Set.Set (CType, CType)
addPairType t s =
    case t of
        CTPtr (CTPair tx ty) -> Set.insert (tx, ty) s
        CTPair tx ty -> Set.insert (tx, ty) s
        _ -> s

-- addPairType :: CType -> FunctionInfo -> FunctionInfo
-- addPairType t m =
--     case t of
--         CTPtr (CTPair tx ty) -> m {pairTypesGlobal = Set.insert (tx, ty) (pairTypesGlobal m)}
--         CTPair tx ty -> m {pairTypesGlobal = Set.insert (tx, ty) (pairTypesGlobal m)}
--         _ -> m

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
getGlobalInfo (AllocEnv _ i directPs _) m =
    let m' = m { usedEnvsGlobal = Set.insert i (usedEnvsGlobal m) }
    in foldr (\(CArg _ x) acc -> getGlobalInfoExpr x acc) m' directPs
getGlobalInfo (Seq x y) m = getGlobalInfo y (getGlobalInfo x m)
getGlobalInfo (If c x y) m = getGlobalInfo y (getGlobalInfo x (getGlobalInfoExpr c m))
getGlobalInfo (While c x) m = getGlobalInfo x (getGlobalInfoExpr c m)
getGlobalInfo (DefFun t _ params body) m =
    let m' = foldr (\p acc ->
                case p of
                    CParamEnv i -> m { usedEnvsGlobal = Set.insert i (usedEnvsGlobal m) }
                    _ -> acc ) m params
        m'' = m' { pairTypesGlobal = addPairType t (pairTypesGlobal m') }
    in getGlobalInfo body m''
getGlobalInfo (Return x) m = getGlobalInfoExpr x m
getGlobalInfo (DefVar t i x) m =
    let m' = addAlias x i t m
    in getGlobalInfoExpr x (m' {pairTypesGlobal = addPairType t (pairTypesGlobal m)})
getGlobalInfo (UpdateVar t _ x) m = getGlobalInfoExpr x (m {pairTypesGlobal = addPairType t (pairTypesGlobal m)})
getGlobalInfo _ m = m

getGlobalInfoExpr :: CExpression -> GlobalInfo -> GlobalInfo
getGlobalInfoExpr (Val (EnvV i)) m = m { usedEnvsGlobal = Set.insert i (usedEnvsGlobal m)}
getGlobalInfoExpr (GetEnvField t i _) m = m { usedEnvsGlobal = Set.insert i (usedEnvsGlobal m), pairTypesGlobal = addPairType t (pairTypesGlobal m) }
getGlobalInfoExpr (CallExpr tf tx f x) m =
    let (func, args) = CDefs.collectArgs (CallExpr tf tx f x)
        m' = case func of
                Var _ i -> m { funCallsGlobal = Map.insertWith (+) i 1 (funCallsGlobal m)}
                _ -> getGlobalInfoExpr f m
        m'' = m' {pairTypesGlobal = addPairType tx (addPairType tf (pairTypesGlobal m'))}
    in foldr (\(CArg _ a) acc -> getGlobalInfoExpr a acc) m'' args
getGlobalInfoExpr (ApplyClosure t f x) m = getGlobalInfoExpr x (getGlobalInfoExpr f (m {pairTypesGlobal = addPairType t (pairTypesGlobal m)}))
getGlobalInfoExpr (Ternary tp c t e) m = getGlobalInfoExpr e (getGlobalInfoExpr t (getGlobalInfoExpr c (m {pairTypesGlobal = addPairType tp (pairTypesGlobal m)})))
getGlobalInfoExpr (ConsList t x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x (m {pairTypesGlobal = addPairType t (pairTypesGlobal m)}))
getGlobalInfoExpr (Prod t x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x (m {pairTypesGlobal = addPairType t (pairTypesGlobal m)}))
getGlobalInfoExpr (Fst t t2 x) m = getGlobalInfoExpr x (m {pairTypesGlobal = addPairType t2 (addPairType t (pairTypesGlobal m))})
getGlobalInfoExpr (Snd t t2 x) m = getGlobalInfoExpr x (m {pairTypesGlobal = addPairType t2 (addPairType t (pairTypesGlobal m))})
getGlobalInfoExpr (IsEmpty t x) m = getGlobalInfoExpr x (m {pairTypesGlobal = addPairType t (pairTypesGlobal m)})
getGlobalInfoExpr (HeadList t x) m = getGlobalInfoExpr x (m {pairTypesGlobal = addPairType t (pairTypesGlobal m)})
getGlobalInfoExpr (TailList t x) m = getGlobalInfoExpr x (m {pairTypesGlobal = addPairType t (pairTypesGlobal m)})
getGlobalInfoExpr (Box t x) m = getGlobalInfoExpr x (m {pairTypesGlobal = addPairType t (pairTypesGlobal m)})
getGlobalInfoExpr (Unbox t x) m = getGlobalInfoExpr x (m {pairTypesGlobal = addPairType t (pairTypesGlobal m)})
getGlobalInfoExpr (CastExpr t x) m = getGlobalInfoExpr x (m {pairTypesGlobal = addPairType t (pairTypesGlobal m)})
getGlobalInfoExpr (IndexList t x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x (m {pairTypesGlobal = addPairType t (pairTypesGlobal m)}))
getGlobalInfoExpr (LIntOp _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr (LCmpOp _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr (LBoolOp _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr (Not x) m = getGlobalInfoExpr x m
getGlobalInfoExpr (Abs x) m = getGlobalInfoExpr x m
getGlobalInfoExpr _ m = m

-- Per function body AST info traversal

emptyFunctionInfo :: FunctionInfo
emptyFunctionInfo = FunctionInfo Set.empty Map.empty Map.empty Set.empty Set.empty Set.empty Map.empty Set.empty

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

getFunctionInfoExpr :: Bool -> CExpression -> FunctionInfo -> FunctionInfo
getFunctionInfoExpr _ (GetEnvField _ envId _) r = r { envUses = Set.insert envId (envUses r) }
getFunctionInfoExpr escapes (Var _ i) r =
    if escapes
    then r { escapedVars = Set.insert i (escapedVars r), varUses = Map.insertWith (+) i 1 (varUses r) }
    else r { varUses = Map.insertWith (+) i 1 (varUses r) }
getFunctionInfoExpr escapes (Val (EnvV i)) r =
    if escapes
    then r { escapedEnvs = Set.insert i (escapedEnvs r), envUses = Set.insert i (envUses r)  }
    else r { envUses = Set.insert i (envUses r)  }
getFunctionInfoExpr escapes (Val (ClosureV i)) r =
    if escapes
    then r { escapedClos = Set.insert i (escapedClos r), varUses = Map.insertWith (+) i 1 (varUses r) }
    else r { varUses = Map.insertWith (+) i 1 (varUses r) }
getFunctionInfoExpr escapes (CallExpr tf tx f x) m =
    let (func, args) = CDefs.collectArgs (CallExpr tf tx f x)
        m' = case func of
                Var _ i -> m { functionCalls = Map.insertWith (+) i 1 (functionCalls m) }
                _ -> getFunctionInfoExpr escapes f m
    in foldr (\(CArg _ a) acc -> processArg a acc) m' args
    where
        processArg (Val (EnvV _)) m' = m'
        processArg arg m' = getFunctionInfoExpr True arg m'
getFunctionInfoExpr escapes (ApplyClosure _ f x) m = getFunctionInfoExpr False f (getFunctionInfoExpr escapes x m)
getFunctionInfoExpr escape e m =  foldl' (flip (getFunctionInfoExpr escape)) m (childrenExpr e)

getFunctionInfo :: CStatement -> FunctionInfo -> FunctionInfo
getFunctionInfo (DefFun _ _ _ body) r = getFunctionInfo body r
getFunctionInfo (Seq x y) r = getFunctionInfo y (getFunctionInfo x r)
getFunctionInfo (Return x) r = getFunctionInfoExpr True x r
getFunctionInfo (If c t e) r =
    let r' = getFunctionInfoExpr False c r
    in mergeFunctionInfo (getFunctionInfo t r') (getFunctionInfo e r')
getFunctionInfo (UpdateVar t i x) r = getFunctionInfoExpr False x (r { varUses = Map.insertWith (+) i 1 (varUses r), varDefs = Map.insert i (CArg t x) (varDefs r)})
getFunctionInfo (DefVar t i x) r = getFunctionInfoExpr False x (r { varUses = Map.insert i 0 (varUses r), varDefs = Map.insert i (CArg t x) (varDefs r) })
getFunctionInfo (While c x) r = getFunctionInfo x (getFunctionInfoExpr False c r)
getFunctionInfo (AllocEnv i parentId directPs parentPs) r =
    let r' = r { allocedEnvs = Set.insert i (allocedEnvs r), envUses = if null parentPs then envUses r else Set.insert parentId (envUses r) }
    in foldr (\(CArg _ x) acc -> getFunctionInfoExpr False x acc) r' directPs
getFunctionInfo (AllocClosure i) r =
    r { envUses = Set.insert i (envUses r), escapedEnvs = Set.insert i (escapedEnvs r)}
getFunctionInfo _ r = r
