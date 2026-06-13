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
scanUsesExpr :: CExpression a -> Map.Map Int Bool
scanUsesExpr (Fst _ _ (Var _ i)) = Map.singleton i True
scanUsesExpr (Snd _ _ (Var _ i)) = Map.singleton i True
scanUsesExpr (Var _ i) = Map.singleton i False  -- bare use is bad
scanUsesExpr e = foldr (Map.unionWith (&&)) Map.empty [scanUsesExpr c | Some c <- childrenExpr e]

scanUses :: CStatement a -> Map.Map Int Bool
scanUses s =
    foldr (Map.unionWith (&&)) Map.empty
        ([scanUses c | SomeStmt c <- childrenStmt s] ++ [scanUsesExpr e | Some e <- childExprsStmt s])

collectPairParam :: CParam -> Map.Map Int CType
collectPairParam (CParam i t) | isPair t = Map.singleton i t
collectPairParam _ = Map.empty

collectPairLocals :: CStatement a -> Map.Map Int CType
collectPairLocals (DefVar t i _) | isPair t = Map.singleton i t
collectPairLocals (BindExpr t _ i k)
    | isPair t = Map.insert i t (collectPairLocals k)
    | otherwise = collectPairLocals k
collectPairLocals (DefFun _ _ params b) = Map.union (collectPairLocals b) (Map.unions $ map collectPairParam params)
collectPairLocals s = foldr Map.union Map.empty ([collectPairLocals c | SomeStmt c <- childrenStmt s])

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
returnIsAlwaysStoredExpr ifun e = and [returnIsAlwaysStoredExpr ifun c | Some c <- childrenExpr e]

returnIsAlwaysStored :: Int -> CStatement a -> Bool
returnIsAlwaysStored ifun (DefVar _ _ expr@CallExpr{}) =
    let (_, args) = collectArgs expr
    in all (\(CArg _ x) -> returnIsAlwaysStoredExpr ifun x) args
returnIsAlwaysStored ifun (UpdateVar _ _ expr@CallExpr{}) =
    let (_, args) = collectArgs expr
    in all (\(CArg _ x) -> returnIsAlwaysStoredExpr ifun x) args
returnIsAlwaysStored ifun (BindExpr _ expr@CallExpr{} _ y) =
    let (_, args) = collectArgs expr
    in all (\(CArg _ x) -> returnIsAlwaysStoredExpr ifun x) args && returnIsAlwaysStored ifun y
returnIsAlwaysStored ifun s = and ([returnIsAlwaysStored ifun c | SomeStmt c <- childrenStmt s] ++ [returnIsAlwaysStoredExpr ifun e | Some e <- childExprsStmt s])

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
demotePairs (DefFun tret ifun params body) m funs = 
    let p' = [case p of CParam i t -> CParam i (stripPairPtr i t m); _ -> p | p <- params]
    in DefFun (stripPairPtr ifun tret m) ifun p' (demotePairs body m funs)
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
demotePairs s m funs = mapChildrenStmt (\x -> demotePairs x m funs) (\e -> demotePairsExpr e m funs) s

demotePairsExpr :: CExpression a -> Map.Map Int Bool -> Map.Map Int [Int] -> CExpression a
demotePairsExpr (Var t i) m _ = Var (stripPairPtr i t m) i
demotePairsExpr (Fst tp tr (Var tv i)) m _ = Fst (stripPairPtr i tp m) tr (Var tv i)
demotePairsExpr (Snd tp tr (Var tv i)) m _ = Snd (stripPairPtr i tp m) tr (Var tv i)
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
demotePairsExpr e m funs = mapChildrenExpr (\x -> demotePairsExpr x m funs) e
