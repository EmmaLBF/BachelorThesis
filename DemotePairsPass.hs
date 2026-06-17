{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DemotePairsPass where
import C
import Utils
import AST
import CDefs
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State


-- STACK ALLOCATE PAIRS

canBeByValueFun :: Int -> CStatement -> Map.Map Int Bool -> Bool
canBeByValueFun ifun body varsByValue =
    let isAlwaysStored = returnIsAlwaysStored ifun body
        varsThatHoldReturn = getVarsThatHoldReturn ifun body
    in isAlwaysStored && all (\i -> case Map.lookup i varsByValue of
                                        Just True -> True
                                        _ -> False) varsThatHoldReturn

getValueFunsSet :: [Int] -> CStatement -> Map.Map Int Bool -> Map.Map Int Bool
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
canBeByValue :: CStatement -> Map.Map Int Bool
canBeByValue stmt =
    let pairLocals = collectPairLocals stmt   -- vars whose DefVar type is pair
        usage = scanUses stmt
        varsbyValue = Map.mapWithKey (\i _ -> Map.findWithDefault False i usage) pairLocals
        funs = getFunsWithParams stmt
        funsByValue = getValueFunsSet (Map.keys funs) stmt varsbyValue
    in Map.unionWith (||) varsbyValue funsByValue

-- if a pair is used not as fst/snd anywhere it is 'bad'
-- check pair usage (true -> only fst/snd, false -> used by itself)
scanUsesExpr :: CExpression -> Map.Map Int Bool
scanUsesExpr (Fst _ _ (Var _ i)) = Map.singleton i True
scanUsesExpr (Snd _ _ (Var _ i)) = Map.singleton i True
scanUsesExpr (Var _ i) = Map.singleton i False  -- bare use is bad
scanUsesExpr e = foldr (Map.unionWith (&&)) Map.empty [scanUsesExpr c | c <- childrenExpr e]

scanUses :: CStatement -> Map.Map Int Bool
scanUses s =
    foldr (Map.unionWith (&&)) Map.empty
        ([scanUses c | c <- childrenStmt s] ++ [scanUsesExpr e | e <- childExprsStmt s])

collectPairParam :: CParam -> Map.Map Int CType
collectPairParam (CParam i t) | isPair t = Map.singleton i t
collectPairParam _ = Map.empty

collectPairLocals :: CStatement -> Map.Map Int CType
collectPairLocals (DefVar t i _) | isPair t = Map.singleton i t
collectPairLocals (BindExpr t _ i k)
    | isPair t = Map.insert i t (collectPairLocals k)
    | otherwise = collectPairLocals k
collectPairLocals (DefFun _ _ params b) = Map.union (collectPairLocals b) (Map.unions $ map collectPairParam params)
collectPairLocals s = foldr Map.union Map.empty ([collectPairLocals c | c <- childrenStmt s])

-- a function return type can be demoted if every use of the return value of said function
-- is stored in a var that can also be demoted

-- returns true if the result of call this function is only ever stored in a var
    -- through defvars/updatevars/binds directly
returnIsAlwaysStoredExpr :: Int -> CExpression -> Bool
returnIsAlwaysStoredExpr ifun expr@CallExpr{} =
    let (f', _) = collectArgs expr
    in case f' of
        Var _ i' | i' == ifun -> False
        _ -> True
returnIsAlwaysStoredExpr ifun e = and [returnIsAlwaysStoredExpr ifun c | c <- childrenExpr e]

returnIsAlwaysStored :: Int -> CStatement -> Bool
returnIsAlwaysStored ifun (DefVar _ _ expr@CallExpr{}) =
    let (_, args) = collectArgs expr
    in all (\(CArg _ x) -> returnIsAlwaysStoredExpr ifun x) args
returnIsAlwaysStored ifun (UpdateVar _ _ expr@CallExpr{}) =
    let (_, args) = collectArgs expr
    in all (\(CArg _ x) -> returnIsAlwaysStoredExpr ifun x) args
returnIsAlwaysStored ifun (BindExpr _ expr@CallExpr{} _ y) =
    let (_, args) = collectArgs expr
    in all (\(CArg _ x) -> returnIsAlwaysStoredExpr ifun x) args && returnIsAlwaysStored ifun y
returnIsAlwaysStored ifun s = and ([returnIsAlwaysStored ifun c | c <- childrenStmt s] ++ [returnIsAlwaysStoredExpr ifun e | e <- childExprsStmt s])

isCallToFun :: Int -> CExpression -> Bool
isCallToFun ifun expr@CallExpr{} =
    let (f', _) = collectArgs expr
    in case f' of
        Var _ i' | i' == ifun -> True
        _ -> False
isCallToFun _ _ = False


-- get all the defvars/updatevars/binds that hold a return of that function
getVarsThatHoldReturn :: Int -> CStatement -> Set.Set Int
getVarsThatHoldReturn ifun (DefVar _ i expr@CallExpr{}) | isCallToFun ifun expr = Set.singleton i
getVarsThatHoldReturn ifun (UpdateVar _ i expr@CallExpr{}) | isCallToFun ifun expr = Set.singleton i
getVarsThatHoldReturn ifun (BindExpr _ expr@CallExpr{} i y) | isCallToFun ifun expr = Set.insert i (getVarsThatHoldReturn ifun y)
getVarsThatHoldReturn ifun s = foldr Set.union Set.empty ([getVarsThatHoldReturn ifun c | c <- childrenStmt s])


-- put pairs on stack

-- turns pair pointer into just pair if it can be demoted
stripPairPtr :: Int -> CType -> Map.Map Int Bool -> CType
stripPairPtr i (CTPtr (CTPair tl tr)) m
    | Map.lookup i m == Just True = CTPair tl tr
stripPairPtr _ t _ = t

-- need to explicitly strip prod of its type because it does not know what variable its held in
demotePairs :: CStatement -> Map.Map Int Bool -> Map.Map Int [Int] -> CStatement
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

demotePairsExpr :: CExpression -> Map.Map Int Bool -> Map.Map Int [Int] -> CExpression
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



-- closure id, parent id, aplly to rewrite
-- turn the application of a heap allocated closure into the call of a stack allocated one
-- We call the fn stored in the closure directly with the env and its args
-- i is the id of the closureAlloc were getting rid of
    -- if we find the application of that closure we need to rewrite it to a callexpr
-- also we need to remove the cast for the apply
rewriteClosureUseExpr :: Int -> CExpression -> CExpression
rewriteClosureUseExpr i (ApplyClosure targ f arg) =
    let (f', args) = collectArgsApply (ApplyClosure targ f arg)
    in case f' of
        Val (ClosureV i') | i == i' ->
            let args' = map (\(CArg t x) -> CArg t (rewriteClosureUseExpr i x)) args
                envArg = CArg CTVoidPtr (Val (EnvV i))
            in rebuildCall CTVoidPtr (Var CTVoidPtr i) (envArg : args') -- call the closure function directly
        _ -> ApplyClosure targ (rewriteClosureUseExpr i f) (rewriteClosureUseExpr i arg)
rewriteClosureUseExpr i (CastExpr t x@ApplyClosure{}) =
    let (f', _) = collectArgsApply x
    in case f' of
        Val (ClosureV i') | i == i' -> rewriteClosureUseExpr i x -- get rid of cast
        _ -> CastExpr t (rewriteClosureUseExpr i x)
rewriteClosureUseExpr i e = mapChildrenExpr (rewriteClosureUseExpr i) e

-- remove the alloc closures we don't need and the envs that have no direct params
rewriteClosureUse :: Int -> CStatement -> CStatement
rewriteClosureUse i = mapChildrenStmt (rewriteClosureUse i) (rewriteClosureUseExpr i)

-- top level pass, if we alloc a closure that never escapes the function we can keep it on the stack
-- collects a list of closures that are local to the function they are in
    -- it then removes all of these from the body of that function
demoteClosures :: CStatement -> FunctionInfo -> State [Int] CStatement
demoteClosures (AllocClosure i) g
    | i `notElem` escapedClos g = Skip <$ modify (i :)
demoteClosures (DefFun tret ifun ps x) _ =
    let (x', r) = runState (demoteClosures x (getFunctionInfo x emptyFunctionInfo)) []
        x'' = foldr rewriteClosureUse x' r
    in return $ DefFun tret ifun ps x''
demoteClosures x g = mapChildrenStmtM (`demoteClosures` g) pure x

