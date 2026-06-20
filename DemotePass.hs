{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DemotePass where
import C
import Utils
import AST
import CDefs
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe


-- STACK ALLOCATE PAIRS

-- function return value can be stack allocated if its value is always stored in a variable
    -- and that variable is always used by value
canBeByValueFun :: Int -> CStatement -> VarsByValue -> Bool
canBeByValueFun ifun body varsByValue =
    let isAlwaysStored = returnIsAlwaysStored ifun body
        varsThatHoldReturn = getVarsThatHoldReturn ifun body
    in isAlwaysStored && all (`elem` varsByValue) varsThatHoldReturn

-- all the variables which escape a demoted function will now also be demoted
getFunsByValue :: Int -> VarsByValue -> CStatement -> FunsByValue
getFunsByValue i varsByValue body 
    | canBeByValueFun i body varsByValue =
        let funDef = fromMaybe Skip (findFunDef i (getDefs body))
            analysis = getFunctionInfo funDef emptyFunctionInfo
        in foldr Set.insert (Set.singleton i) (escapedVars analysis)
    | otherwise = Set.empty

-- A pair is safe to put of stack iff every use of it is only with fst or snd
canBeByValue :: CStatement -> VarsByValue
canBeByValue stmt =
    let pairLocals = collectPairLocals stmt   -- vars whose DefVar type is pair
        fstSndVars = Map.keysSet $ Map.filter id (alwaysFstSnd stmt) -- vars who are always used only with fst/snd
        varsbyValue = pairLocals `Set.intersection` fstSndVars -- vars that are used by value
        funsByValue = foldr (\i -> Set.union (getFunsByValue i varsbyValue stmt)) Set.empty (getDefIds stmt)
    in Set.union varsbyValue funsByValue

-- if a pair is used not as fst/snd anywhere it is 'bad'
-- check pair usage (true -> only fst/snd, false -> used by itself)
alwaysFstSndExpr :: CExpression -> Map.Map Int Bool
alwaysFstSndExpr (Fst _ _ (Var _ i)) = Map.singleton i True
alwaysFstSndExpr (Snd _ _ (Var _ i)) = Map.singleton i True
alwaysFstSndExpr (Var _ i) = Map.singleton i False  -- bare use is bad
alwaysFstSndExpr e = foldr (Map.unionWith (&&)) Map.empty [alwaysFstSndExpr c | c <- childrenExpr e]

alwaysFstSnd :: CStatement -> Map.Map Int Bool
alwaysFstSnd s = foldr (Map.unionWith (&&)) Map.empty ([alwaysFstSnd c | c <- childrenStmt s] ++ [alwaysFstSndExpr e | e <- childExprsStmt s])

collectPairParam :: CParam -> Set.Set Int
collectPairParam (CParam i t) | isPair t = Set.singleton i
collectPairParam _ = Set.empty

collectPairLocals :: CStatement -> Set.Set Int
collectPairLocals (DefVar t i _) | isPair t = Set.singleton i
collectPairLocals (DefFun _ _ params b) = Set.union (collectPairLocals b) (Set.unions $ map collectPairParam params)
collectPairLocals s = foldr Set.union Set.empty ([collectPairLocals c | c <- childrenStmt s])

-- returns true if the result of call this function is only ever stored in a var
    -- through defvars/updatevars/binds directly
returnIsAlwaysStoredExpr :: Int -> CExpression -> Bool
returnIsAlwaysStoredExpr ifun expr@CallExpr{} = not (isCallToFun ifun expr)
returnIsAlwaysStoredExpr ifun e = and [returnIsAlwaysStoredExpr ifun c | c <- childrenExpr e]

returnIsAlwaysStored :: Int -> CStatement -> Bool
returnIsAlwaysStored ifun (DefVar _ _ expr@CallExpr{}) =
    let (_, args) = collectArgs expr
    in all (\(CArg _ x) -> returnIsAlwaysStoredExpr ifun x) args
returnIsAlwaysStored ifun (UpdateVar _ _ expr@CallExpr{}) =
    let (_, args) = collectArgs expr
    in all (\(CArg _ x) -> returnIsAlwaysStoredExpr ifun x) args
returnIsAlwaysStored ifun s = and ([returnIsAlwaysStored ifun c | c <- childrenStmt s] ++ [returnIsAlwaysStoredExpr ifun e | e <- childExprsStmt s])

isCallToFun :: Int -> CExpression -> Bool
isCallToFun ifun expr@CallExpr{} =
    let (f', _) = collectArgs expr
    in case f' of Var _ i' | i' == ifun -> True; _ -> False
isCallToFun _ _ = False

-- get all the defvars/updatevars/binds that hold a return of that function
getVarsThatHoldReturn :: Int -> CStatement -> Set.Set Int
getVarsThatHoldReturn ifun (DefVar _ i expr@CallExpr{}) | isCallToFun ifun expr = Set.singleton i
getVarsThatHoldReturn ifun (UpdateVar _ i expr@CallExpr{}) | isCallToFun ifun expr = Set.singleton i
getVarsThatHoldReturn ifun s = foldr Set.union Set.empty ([getVarsThatHoldReturn ifun c | c <- childrenStmt s])


-- put pairs on stack

-- turns pair pointer into just pair if it can be demoted
stripPairPtr :: Int -> CType -> VarsByValue -> CType
stripPairPtr i (CTPtr (CTPair tl tr)) m | i `elem` m = CTPair tl tr
stripPairPtr _ t _ = t

-- need to explicitly strip prod of its type because it does not know what variable its held in
demotePairsStmt :: CStatement -> VarsByValue -> Map.Map Int [Int] -> CStatement
demotePairsStmt (DefFun tret ifun params body) m funs =
    let p' = [case p of CParam i t -> CParam i (stripPairPtr i t m); _ -> p | p <- params]
    in DefFun (stripPairPtr ifun tret m) ifun p' (demotePairsStmt body m funs)
demotePairsStmt (DefVar t i (Prod tp x y)) m funs = DefVar (stripPairPtr i t m) i (Prod (stripPairPtr i tp m) (demotePairsExpr x m funs) (demotePairsExpr y m funs))
demotePairsStmt (DefVar t i (Val UnitV)) m _ = -- default val for demoted pair is no longer NULL
    let t' = stripPairPtr i t m
    in DefVar t' i (Val (defaultVal t'))
demotePairsStmt (DefVar t i x) m funs =
    let t' = stripPairPtr i t m
        x' = demotePairsExpr x m funs
    in  if t == t' then DefVar t' i x'
        else case getTypeExpr x' of
            (CTPtr _) -> DefVar t' i (Unbox t' x')
            CTList -> DefVar t' i (Unbox t' x')
            _ -> DefVar t' i x'
demotePairsStmt (UpdateVar t i (Prod tp x y)) m funs = UpdateVar (stripPairPtr i t m) i (Prod (stripPairPtr i tp m) (demotePairsExpr x m funs) (demotePairsExpr y m funs))
demotePairsStmt s m funs = mapChildrenStmt (\x -> demotePairsStmt x m funs) (\e -> demotePairsExpr e m funs) s

demotePairsArgs :: [(CArg, Int)] -> VarsByValue -> Map.Map Int [Int] -> CArgs
demotePairsArgs [] _ _ = []
demotePairsArgs [(CArg _ (Prod tp l r), idProd)] m funs 
    | idProd `elem` m = [CArg tp (Prod (stripPairPtr idProd tp m) (demotePairsExpr l m funs) (demotePairsExpr r m funs))]
demotePairsArgs [(CArg targ x, _)] m funs = [CArg targ (demotePairsExpr x m funs)]
demotePairsArgs (b:is) m funs = demotePairsArgs [b] m funs ++ demotePairsArgs is m funs

demotePairsExpr :: CExpression -> VarsByValue -> Map.Map Int [Int] -> CExpression
demotePairsExpr (Var t i) m _ = Var (stripPairPtr i t m) i
demotePairsExpr (Fst tp tr (Var tv i)) m _ = Fst (stripPairPtr i tp m) tr (Var tv i)
demotePairsExpr (Snd tp tr (Var tv i)) m _ = Snd (stripPairPtr i tp m) tr (Var tv i)
demotePairsExpr (CallExpr tf tx f a) m funs =
    let (f', args) = collectArgs (CallExpr tf tx f a)
        fId = case f' of Var _ i -> i; _ -> -1
        fArgIds = Map.findWithDefault [] fId funs
        args' = demotePairsArgs (zip args (fArgIds ++ repeat (-1))) m funs -- padded so it takes all the args
    in rebuildCall tf f' args'
demotePairsExpr e m funs = mapChildrenExpr (\x -> demotePairsExpr x m funs) e

demotePairsPass :: CStatement -> CStatement
demotePairsPass s =
    let byValueSet = canBeByValue s
        funsWithParams = getFunsWithParams s
    in demotePairsStmt s byValueSet funsWithParams





-- **** DEMOTE CLOSURES ****

-- closure id, parent id, aplly to rewrite
-- turn the application of a heap allocated closure into the call of a stack allocated one
-- We call the fn stored in the closure directly with the env and its args
-- i is the id of the closureAlloc were getting rid of
    -- if we find the application of that closure we need to rewrite it to a callexpr
rewriteClosureUseExpr :: RemovedClos -> CExpression -> CExpression
rewriteClosureUseExpr removed e@ApplyClosure{} =
    let (f', args) = collectArgsApply e
    in case f' of
        Val (ClosureV i') | i' `elem` removed ->
            let args' = map (\(CArg t x) -> CArg t (rewriteClosureUseExpr removed x)) args
                envArg = CArg (CTPtr CTVoid) (Val (EnvV i'))
            in rebuildCall (CTPtr CTVoid) (Var (CTPtr CTVoid) i') (envArg : args') -- call the closure function directly
        _ -> mapChildrenExpr (rewriteClosureUseExpr removed) e
rewriteClosureUseExpr i e = mapChildrenExpr (rewriteClosureUseExpr i) e

-- remove the alloc closures we don't need and the envs that have no direct params
rewriteClosureUse :: RemovedClos -> CStatement -> CStatement
rewriteClosureUse r (AllocClosure i) | i `elem` r = Skip
rewriteClosureUse r s = mapChildrenStmt (rewriteClosureUse r) (rewriteClosureUseExpr r) s

collectedDemotedClosures :: CStatement -> FunctionInfo -> RemovedClos
collectedDemotedClosures (AllocClosure i) g | i `notElem` escapedClos g = Set.singleton i
collectedDemotedClosures s g = foldr Set.union Set.empty ([collectedDemotedClosures c g | c <- childrenStmt s])

-- top level pass, if we alloc a closure that never escapes the function we can keep it on the stack
-- collects a list of closures that are local to the function they are in
    -- it then removes all of these from the body of that function
demoteClosures :: CStatement -> FunctionInfo -> CStatement
demoteClosures (DefFun tret ifun ps x) _ =
    let r = collectedDemotedClosures x (getFunctionInfo x emptyFunctionInfo)
    in DefFun tret ifun ps (rewriteClosureUse r x)
demoteClosures x g = mapChildrenStmt (`demoteClosures` g) id x


-- ***** REMOVE LOCAL ENVS ***** 

-- removes envs that are only used locally (no alloc needed)
demoteEnvs :: CStatement -> CStatement
demoteEnvs (DefFun t i p body) =
    let removed = collectUnusedEnvAllocs body emptyFunctionInfo
    in DefFun t i p (rewriteRemovedEnvs removed body)
demoteEnvs (Seq x y) = Seq (demoteEnvs x) (demoteEnvs y)
demoteEnvs x = x

-- PASS 1: remove the AllocEnv statements that aren't needed, collect their ids
    -- can only be removed if it does not escape anywhere
collectUnusedEnvAllocs :: CStatement -> FunctionInfo -> RemovedEnvs
collectUnusedEnvAllocs (AllocEnv envId _ _) r | envId `notElem` escapedEnvs r = Set.singleton envId
collectUnusedEnvAllocs s@(DefFun _ _ _ body) _ =
    collectUnusedEnvAllocs body (getFunctionInfo s emptyFunctionInfo)
collectUnusedEnvAllocs s r = foldr Set.union Set.empty ([collectUnusedEnvAllocs c r | c <- childrenStmt s])

-- PASS 2: rewrites get-env-field accesses to the envs that were removed, and removes alloc statements
rewriteRemovedEnvs :: RemovedEnvs -> CStatement -> CStatement
rewriteRemovedEnvs r (AllocEnv envId _ _) | envId `elem` r = Skip
rewriteRemovedEnvs r s = mapChildrenStmt (rewriteRemovedEnvs r) (rewriteRemovedEnvsExpr r) s

rewriteRemovedEnvsExpr :: RemovedEnvs -> CExpression -> CExpression
rewriteRemovedEnvsExpr removed (GetEnvField t envId varId)
    | Set.member envId removed = rewriteRemovedEnvsExpr removed (Var t varId)
    | otherwise = GetEnvField t envId varId
rewriteRemovedEnvsExpr removed e = mapChildrenExpr (rewriteRemovedEnvsExpr removed) e
