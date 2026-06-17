{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AST where

import CDefs
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Global AST info traversal

addPairType :: CType -> Set.Set (CType, CType) -> Set.Set (CType, CType)
addPairType t s =
    case t of
        CTPtr (CTPair tx ty) -> Set.insert (tx, ty) s
        CTPair tx ty -> Set.insert (tx, ty) s
        _ -> s

emptyGlobalInfo :: GlobalInfo
emptyGlobalInfo = GlobalInfo Set.empty Map.empty Map.empty Set.empty Map.empty Map.empty Set.empty

mergeGlobalInfo :: GlobalInfo -> GlobalInfo -> GlobalInfo
mergeGlobalInfo a b = GlobalInfo
    (Set.union (usedEnvs a) (usedEnvs b))
    (Map.unionWith (+) (closureUses a) (closureUses b))
    (Map.unionWith (+) (functionCallsGlobal a) (functionCallsGlobal b))
    (Set.union (globalUsedVars a) (globalUsedVars b))
    (Map.union (aliases a) (aliases b))
    (Map.unionWith (++) (callArgs a) (callArgs b))
    (Set.union (pairTypes a) (pairTypes b))

getGlobalInfo :: CStatement -> GlobalInfo -> GlobalInfo
getGlobalInfo (AllocEnv _ i directPs _) m =
    let m' = m { usedEnvs = Set.insert i (usedEnvs m) }
    in foldr (\(CArg _ x) acc -> getGlobalInfoExpr x acc) m' directPs
getGlobalInfo (Seq x y) m = getGlobalInfo y (getGlobalInfo x m)
getGlobalInfo (If c x y) m = getGlobalInfo y (getGlobalInfo x (getGlobalInfoExpr c m))
getGlobalInfo (While c x) m = getGlobalInfo x (getGlobalInfoExpr c m)
getGlobalInfo (DefFun t _ params body) m =
    let m' = foldr (\p acc ->
                case p of
                    CParamEnv i -> m { usedEnvs = Set.insert i (usedEnvs m) }
                    _ -> acc ) m params
        m'' = m' { pairTypes = addPairType t (pairTypes m') }
    in getGlobalInfo body m''
getGlobalInfo (Return x) m = getGlobalInfoExpr x m
getGlobalInfo (DefVar t i x) m =
    let m' = case x of
                Val (EnvV j) -> m { aliases = Map.insert i (CArg t (Val (EnvV j))) (aliases m)}
                Val (ClosureV j) -> m { aliases = Map.insert i (CArg t (Val (ClosureV j))) (aliases m)}
                HeadList t2 var@Var{} -> m { aliases = Map.insert i (CArg t (HeadList t2 var)) (aliases m)}
                TailList t2 var@Var{} -> m { aliases = Map.insert i (CArg t (TailList t2 var)) (aliases m)}
                expr@Var{} -> m { aliases = Map.insert i (CArg t expr) (aliases m)}
                expr@Fst{} -> m { aliases = Map.insert i (CArg t expr) (aliases m)}
                expr@Snd{} -> m { aliases = Map.insert i (CArg t expr) (aliases m)}
                _ -> m
    in getGlobalInfoExpr x (m' {globalUsedVars = Set.insert i (globalUsedVars m'), pairTypes = addPairType t (pairTypes m)})
getGlobalInfo (UpdateVar t i x) m = getGlobalInfoExpr x (m {globalUsedVars = Set.insert i (globalUsedVars m), pairTypes = addPairType t (pairTypes m)})
getGlobalInfo _ m = m

getGlobalInfoExpr :: CExpression -> GlobalInfo -> GlobalInfo
getGlobalInfoExpr (Val (EnvV i)) m = m { usedEnvs = Set.insert i (usedEnvs m), globalUsedVars = Set.insert i (globalUsedVars m)}
getGlobalInfoExpr (Val (ClosureV i)) m = m { closureUses = Map.insertWith (+) i 1 (closureUses m) }
getGlobalInfoExpr (GetEnvField t i _) m = m { usedEnvs = Set.insert i (usedEnvs m), pairTypes = addPairType t (pairTypes m) }
getGlobalInfoExpr (CallExpr tf tx f x) m =
    let (func, args) = CDefs.collectArgs (CallExpr tf tx f x)
        m' = case func of
                Var _ i -> m { functionCallsGlobal = Map.insertWith (+) i 1 (functionCallsGlobal m),
                                callArgs = Map.insertWith (++) i [args] (callArgs m)}
                _ -> getGlobalInfoExpr f m
        m'' = m' {pairTypes = addPairType tx (addPairType tf (pairTypes m'))}
    in foldr (\(CArg _ a) acc -> getGlobalInfoExpr a acc) m'' args
getGlobalInfoExpr (ApplyClosure t f x) m = getGlobalInfoExpr x (getGlobalInfoExpr f (m {pairTypes = addPairType t (pairTypes m)}))
getGlobalInfoExpr (Ternary tp c t e) m = getGlobalInfoExpr e (getGlobalInfoExpr t (getGlobalInfoExpr c (m {pairTypes = addPairType tp (pairTypes m)})))
getGlobalInfoExpr (LIntOp _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr (LCmpOp _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr (LBoolOp _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr (ConsList t x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x (m {pairTypes = addPairType t (pairTypes m)}))
getGlobalInfoExpr (Prod t x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x (m {pairTypes = addPairType t (pairTypes m)}))
getGlobalInfoExpr (Fst t t2 x) m = getGlobalInfoExpr x (m {pairTypes = addPairType t2 (addPairType t (pairTypes m))})
getGlobalInfoExpr (Snd t t2 x) m = getGlobalInfoExpr x (m {pairTypes = addPairType t2 (addPairType t (pairTypes m))})
getGlobalInfoExpr (Not x) m = getGlobalInfoExpr x m
getGlobalInfoExpr (Abs x) m = getGlobalInfoExpr x m
getGlobalInfoExpr (IsEmpty t x) m = getGlobalInfoExpr x (m {pairTypes = addPairType t (pairTypes m)})
getGlobalInfoExpr (HeadList t x) m = getGlobalInfoExpr x (m {pairTypes = addPairType t (pairTypes m)})
getGlobalInfoExpr (TailList t x) m = getGlobalInfoExpr x (m {pairTypes = addPairType t (pairTypes m)})
getGlobalInfoExpr (Box t x) m = getGlobalInfoExpr x (m {pairTypes = addPairType t (pairTypes m)})
getGlobalInfoExpr (Unbox t x) m = getGlobalInfoExpr x (m {pairTypes = addPairType t (pairTypes m)})
getGlobalInfoExpr (CastExpr t x) m = getGlobalInfoExpr x (m {pairTypes = addPairType t (pairTypes m)})
getGlobalInfoExpr (IndexList t x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x (m {pairTypes = addPairType t (pairTypes m)}))
getGlobalInfoExpr _ m = m

-- Per function body AST info traversal

emptyFunctionInfo :: FunctionInfo
emptyFunctionInfo = FunctionInfo 0 [] Set.empty Map.empty Map.empty Set.empty Set.empty Set.empty Map.empty Set.empty

mergeFunctionInfo :: FunctionInfo -> FunctionInfo -> FunctionInfo
mergeFunctionInfo a b = FunctionInfo
    (funId a)
    (funParams a)
    (Set.union (escapedVars a) (escapedVars b))
    (Map.unionWith max (varUses a) (varUses b))
    (Map.union (varDefs a) (varDefs b))
    (Set.union (escapedEnvs a) (escapedEnvs b))
    (Set.union (allocedEnvs a) (allocedEnvs b))
    (Set.union (envUses a) (envUses b))
    (Map.unionWith (+) (functionCalls a) (functionCalls b))
    (Set.union (escapedClos a) (escapedClos b))

getFunctionInfoExpr :: Bool -> CExpression -> FunctionInfo -> FunctionInfo
getFunctionInfoExpr _ (GetEnvField _ envId _) r =
    r { envUses = Set.insert envId (envUses r) }
getFunctionInfoExpr escapes (Var _ i) r =
    if escapes
    then r { escapedVars = Set.insert i (escapedVars r), varUses = Map.insertWith (+) i 1 (varUses r) }
    else r { varUses = Map.insertWith (+) i 1 (varUses r) }
getFunctionInfoExpr escapes (Val (EnvV i)) r =
    if escapes
    then r { escapedEnvs = Set.insert i (escapedEnvs r), envUses = Set.insert i (envUses r)  }
    else r
getFunctionInfoExpr escapes (Val (ClosureV i)) r =
    if escapes
    then r { escapedClos = Set.insert i (escapedClos r), varUses = Map.insertWith (+) i 1 (varUses r) }
    else r { varUses = Map.insertWith (+) i 1 (varUses r) }
getFunctionInfoExpr escapes (Not x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (Abs x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (Fst _ _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (Snd _ _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (IsEmpty _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (HeadList _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (TailList _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (CastExpr _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (Box _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (Unbox _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (Ternary _ _ t e) m = getFunctionInfoExpr escapes t (getFunctionInfoExpr escapes e m)
getFunctionInfoExpr escapes (ConsList _ x y) m = getFunctionInfoExpr escapes x (getFunctionInfoExpr escapes y m)
getFunctionInfoExpr escapes (Prod _ x y) m = getFunctionInfoExpr escapes x (getFunctionInfoExpr escapes y m)
getFunctionInfoExpr escapes (LIntOp _ x y) m = getFunctionInfoExpr escapes  x (getFunctionInfoExpr escapes y m)
getFunctionInfoExpr escapes (LCmpOp _ x y) m = getFunctionInfoExpr escapes x (getFunctionInfoExpr escapes y m)
getFunctionInfoExpr escapes (LBoolOp _ x y) m = getFunctionInfoExpr escapes x (getFunctionInfoExpr escapes y m)
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
getFunctionInfoExpr escapes (IndexList _ x y) m = getFunctionInfoExpr escapes x (getFunctionInfoExpr escapes y m)
getFunctionInfoExpr _ _ m = m

getFunctionInfo :: CStatement -> FunctionInfo -> FunctionInfo
getFunctionInfo (DefFun _ ifun params body) r =
    let r' = getFunctionInfo body (r { funId = ifun, funParams = params})
    in r' { funId = ifun, funParams = params}
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
