{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DemotePairsPass where
import C
import Utils
import AST
import CDefs
import qualified Data.Map as Map
import qualified Data.Set as Set


-- STACK ALLOCATE PAIRS

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
        varsbyValue = Map.mapWithKey (\i _ -> Map.findWithDefault False i usage) pairLocals
        funs = getFunsWithParams stmt
        funsByValue = getValueFunsSet (Map.keys funs) stmt varsbyValue
    in Map.unionWith (||) varsbyValue funsByValue

-- if a pair is used not as fst/snd anywhere it is 'bad'
-- check pair usage (true -> only fst/snd, false -> used by itself)
scanExpr :: CExpression a -> Map.Map Int Bool
scanExpr (Fst _ _ (Var _ i)) = Map.singleton i True
scanExpr (Snd _ _ (Var _ i)) = Map.singleton i True
scanExpr (Var _ i) = Map.singleton i False  -- bare use is bad
scanExpr (Fst _ _ e) = scanExpr e
scanExpr (Snd _ _ e) = scanExpr e
scanExpr (Not x) = scanExpr x
scanExpr (Abs x) = scanExpr x
scanExpr (HeadList _ e) = scanExpr e
scanExpr (TailList _ e) = scanExpr e
scanExpr (IsEmpty _ e) = scanExpr e
scanExpr (CastExpr _ e) = scanExpr e
scanExpr (Box _ e) = scanExpr e
scanExpr (Unbox _ e) = scanExpr e
scanExpr (LIntOp _ x y) = Map.unionWith (&&) (scanExpr x) (scanExpr y)
scanExpr (LCmpOp _ x y) = Map.unionWith (&&) (scanExpr x) (scanExpr y)
scanExpr (LBoolOp _ x y) = Map.unionWith (&&) (scanExpr x) (scanExpr y)
scanExpr (Ternary _ c x y) = Map.unionWith (&&) (scanExpr c) (Map.unionWith (&&) (scanExpr x) (scanExpr y))
scanExpr (ConsList _ x y) = Map.unionWith (&&) (scanExpr x) (scanExpr y)
scanExpr (Prod _ x y) = Map.unionWith (&&) (scanExpr x) (scanExpr y)
scanExpr (CallExpr _ _ f a) = Map.unionWith (&&) (scanExpr f) (scanExpr a)
scanExpr (ApplyClosure _ f a) = Map.unionWith (&&) (scanExpr f) (scanExpr a)
scanExpr _ = Map.empty

scanUses :: CStatement a -> Map.Map Int Bool
scanUses (Return e) = scanExpr e
scanUses (DefVar _ _ e) = scanExpr e
scanUses (DefFun _ _ _ b) = scanUses b
scanUses (UpdateVar _ _ e) = scanExpr e
scanUses (Seq x y) = Map.unionWith (&&) (scanUses x) (scanUses y)
scanUses (While c x) = Map.unionWith (&&) (scanExpr c) (scanUses x)
scanUses (BindExpr _ e _ k) = Map.unionWith (&&) (scanExpr e) (scanUses k)
scanUses (If c x y) = Map.unionWith (&&) (scanExpr c) (Map.unionWith (&&) (scanUses x) (scanUses y))
scanUses _  = Map.empty

collectPairParam :: CParam -> Map.Map Int CType
collectPairParam (CParam i t) | isPair t = Map.singleton i t
collectPairParam _ = Map.empty

collectPairLocals :: CStatement a -> Map.Map Int CType
collectPairLocals (DefVar t i _) | isPair t = Map.singleton i t
collectPairLocals (BindExpr t _ i k)
    | isPair t = Map.insert i t (collectPairLocals k)
    | otherwise = collectPairLocals k
collectPairLocals (Seq x y) = Map.union (collectPairLocals x) (collectPairLocals y)
collectPairLocals (If _ x y) = Map.union (collectPairLocals x) (collectPairLocals y)
collectPairLocals (While _ x) = collectPairLocals x
collectPairLocals (DefFun _ _ params b) = Map.union (collectPairLocals b) (Map.unions $ map collectPairParam params)
collectPairLocals _ = Map.empty

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
        Var _ i' | i' == ifun -> Set.singleton i
        _ -> Set.empty
getVarsThatHoldReturn ifun (UpdateVar _ i expr@CallExpr{}) =
    let (f', _) = collectArgs expr
    in case f' of
        Var _ i' | i' == ifun -> Set.singleton i
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
stripPairPtr i (CTPtr (CTPair tl tr)) m
    | Map.lookup i m == Just True = CTPair tl tr
stripPairPtr _ t _ = t

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
                    demoteArgs (b:is) (id':id'') = demoteArgs [b] [id'] ++ demoteArgs is id''
            _ -> CallExpr tf tx (demotePairsExpr f m funs) (demotePairsExpr a m funs)
    -- CallExpr tf tx (demotePairsExpr f m funs) (demotePairsExpr a m funs)
demotePairsExpr (CastExpr t e) m funs = CastExpr t (demotePairsExpr e m funs)
demotePairsExpr (Box t e) m funs = Box t (demotePairsExpr e m funs)
demotePairsExpr (Unbox t e) m funs = Unbox t (demotePairsExpr e m funs)
demotePairsExpr x _ _ = x
