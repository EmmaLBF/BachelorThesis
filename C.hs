{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}

module C where

import CDefs
import Utils
import AST
import LambdaMergePass
import qualified AbsLang as AL
import qualified NamedLang as NL
import qualified CLang as CL

import Data.Dynamic
import Control.Monad.State
import Data.Typeable

import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )

-- ─────────────────────────────────────────────
--  Translate from CLang
-- ─────────────────────────────────────────────

translateValue :: CL.CValue a -> CValue a
translateValue (CL.IntV x) = IntV x
translateValue (CL.BoolV x) = BoolV x
translateValue CL.UnitV = UnitV
translateValue (CL.PairV x y) = PairV (translateValue x) (translateValue y)
translateValue (CL.FunV f) = FunV (translateValue . f . translateValueBack)
translateValue (CL.ListV x) = ListV (map translateValue x)

translateValueBack :: CValue a -> CL.CValue a
translateValueBack (IntV x) = CL.IntV x
translateValueBack (BoolV x) = CL.BoolV x
translateValueBack UnitV = CL.UnitV
translateValueBack (PairV x y) = CL.PairV (translateValueBack x) (translateValueBack y)
translateValueBack (ListV xs) = CL.ListV (map translateValueBack xs)
translateValueBack (FunV f) = CL.FunV (translateValueBack . f . translateValue)
translateValueBack _ = error "Cannot translate back"

translateExpr :: forall a. CL.CExpression a -> CExpression
translateExpr CL.EmptyList = EmptyList (fromTypeRep (typeRep (Proxy :: Proxy a)))
translateExpr (CL.Not e) = Not (translateExpr e)
translateExpr (CL.Abs e) = Abs (translateExpr e)
translateExpr (CL.Fst (p :: CL.CExpression (b, c))) =
    let tp = fromTypeRep (typeRep (Proxy :: Proxy (b, c)))
        tr = fromTypeRep (typeRep (Proxy :: Proxy b))
    in Fst tp tr (translateExpr p)
translateExpr (CL.Snd (p :: CL.CExpression (b, c))) =
    let tp = fromTypeRep (typeRep (Proxy :: Proxy (b, c)))
        tr = fromTypeRep (typeRep (Proxy :: Proxy c))
    in Snd tp tr (translateExpr p)
translateExpr (CL.Val v) = Val (translateValue v)
translateExpr (CL.IsEmpty (l :: CL.CExpression [b])) = IsEmpty (fromTypeRep (typeRep (Proxy :: Proxy b))) (translateExpr l)
translateExpr (CL.HeadList (l :: CL.CExpression [b])) = HeadList (fromTypeRep (typeRep (Proxy :: Proxy b))) (translateExpr l)
translateExpr (CL.TailList (l :: CL.CExpression [b])) = TailList (fromTypeRep (typeRep (Proxy :: Proxy b))) (translateExpr l)
translateExpr (CL.Prod (f :: CL.CExpression b) ((g :: CL.CExpression c))) =
    let tl = fromTypeRep (typeRep (Proxy :: Proxy b))
        tr = fromTypeRep (typeRep (Proxy :: Proxy c))
    in Prod (CTPtr (CTPair tl tr)) (translateExpr f) (translateExpr g)
translateExpr (CL.Var i) = Var (fromTypeRep (typeRep (Proxy :: Proxy a))) i
translateExpr (CL.ConsList (h :: CL.CExpression b) t) = ConsList (fromTypeRep (typeRep (Proxy :: Proxy b))) (translateExpr h) (translateExpr t)
translateExpr (CL.CallExpr (f :: CL.CExpression func) (x :: CL.CExpression arg)) = CallExpr (fromTypeRep (typeRep (Proxy :: Proxy func)))
    (fromTypeRep (typeRep (Proxy :: Proxy arg))) (translateExpr f) (translateExpr x)
translateExpr (CL.Ternary c t f) = Ternary (fromTypeRep (typeRep (Proxy :: Proxy a))) (translateExpr c) (translateExpr t) (translateExpr f)
translateExpr (CL.IndexList (l :: CL.CExpression [b]) i) = IndexList (fromTypeRep (typeRep (Proxy :: Proxy b))) (translateExpr l) (translateExpr i)
translateExpr (CL.LIntOp op e1 e2) = LIntOp op (translateExpr e1) (translateExpr e2)
translateExpr (CL.LCmpOp op e1 e2) = LCmpOp op (translateExpr e1) (translateExpr e2)
translateExpr (CL.LBoolOp op e1 e2) = LBoolOp op (translateExpr e1) (translateExpr e2)

translate :: CL.CStatement a -> CStatement
translate CL.Skip = Skip
translate (CL.Return x) = Return (translateExpr x)
translate (CL.DefVar i (x :: CL.CExpression a)) =
    DefVar (fromTypeRep (typeRep (Proxy :: Proxy a))) i (translateExpr x)
translate (CL.UpdateVar i (x :: CL.CExpression a)) =
    UpdateVar (fromTypeRep (typeRep (Proxy :: Proxy a))) i (translateExpr x)
translate (CL.While cond x) = While (translateExpr cond) (translate x)
translate (CL.Seq x y) = Seq (translate x) (translate y)
translate (CL.If cond x y) = If (translateExpr cond) (translate x) (translate y)
translate (CL.DefFun ifun (ip, tp) (body:: CL.CStatement b)) =
    DefFun (fromTypeRep (typeRep (Proxy :: Proxy b))) ifun [CParam ip (fromTypeRep (typeRep tp))] (translate body)

-- ─────────────────────────────────────────────
--  Lambda Lifting
-- ─────────────────────────────────────────────

-- (1) for each def search in its body for the first def you find
-- if there is one we lift it out (sequence it before) and remove it from the body
-- the lifted function needs an env as a parameter with the params of all its parents
-- this only lifts one def at a time
-- also returns map of which function is which parent
liftDefs :: CStatement -> State (FreeVars, ParentMap) CStatement
liftDefs stmt@(DefFun tret ifun params body) =
    maybe (return stmt) liftNested (findFirstDefFun body) -- find a def nested inside the current one
    where
    liftNested (DefFun tret' ifun' params' body') = do
        m <- gets fst
        let removedDefOuter = removeDefFun body ifun' -- remove the def we found
            defInner = DefFun tret' ifun' (CParamEnv ifun' : params') body' -- add an env param to it and lift it out
            freeVars = findFreeVars body' params ifun m
        modify $ \(m', n) -> (Map.insert ifun' freeVars m', Map.insert ifun' ifun n) -- add all of its needed params to the map
        return (Seq defInner (DefFun tret ifun params removedDefOuter))
    liftNested _ = error "expected a function definition"
liftDefs (Seq x y) = Seq <$> liftDefs x <*>  liftDefs y
liftDefs x = return x

lambdaLift :: CStatement -> State (FreeVars, ParentMap) CStatement
lambdaLift stmt = do
    (m, _) <- get
    stmt' <- liftDefs stmt
    (m', _) <- get
    let stmt'' = replaceFreeVarAccess stmt' (-1) m'
    if m' /= m then lambdaLift stmt'' else return stmt''

-- free vars are parent free vars + parent params - the variables that arent used
findFreeVars :: CStatement -> CParams -> Int -> FreeVars -> Set.Set CParam
findFreeVars body parentParams ifun freeVars =
    let usedInNested = Map.keysSet (varUses (getFunctionInfo body emptyFunctionInfo)) -- vars that are used in the nested body
        parentVars = Set.union (Set.fromList parentParams) (Map.findWithDefault Set.empty ifun freeVars)
    in Set.filter (\p -> Set.member (paramId p) usedInNested) parentVars 

-- (1a) all of the parent function(s)'s paramteters need to be accessed through the env
-- instead of directly through var
replaceFreeVarAccessExpr :: CExpression -> Int -> FreeVars -> CExpression
replaceFreeVarAccessExpr (Var t i) ifun freeVars =
    case Map.lookup ifun freeVars of
        Just funSet | isElemParamSet i funSet -> GetEnvField t ifun i
        _ -> Var t i
replaceFreeVarAccessExpr e currFun m = mapChildrenExpr (\x -> replaceFreeVarAccessExpr x currFun m) e

replaceFreeVarAccess :: CStatement -> Int -> FreeVars -> CStatement
replaceFreeVarAccess (DefFun t ifun p body) _ m = DefFun t ifun p (replaceFreeVarAccess body ifun m)
replaceFreeVarAccess s currFun m =
    mapChildrenStmt
        (\x -> replaceFreeVarAccess x currFun m)
        (\e -> replaceFreeVarAccessExpr e currFun m) s

-- (2) add env parameters to call sites of hoisted functions
-- if we call a function that is in our lifted set we need to make an env to its call list
addEnvParameterExpr :: CExpression -> FreeVars -> CExpression
addEnvParameterExpr e@(CallExpr tf _ _ _) m =
    let (f', args) = CDefs.collectArgs e
        fId = case f' of (Var _ i) -> i; _ -> -1
    in  if Map.member fId m 
        then rebuildCall tf f' (CArg (CTPtr CTVoid) (Val (EnvV fId)) : map (\(CArg t arg)-> CArg t (addEnvParameterExpr arg m)) args)
        else mapChildrenExpr (`addEnvParameterExpr` m) e
addEnvParameterExpr e m = mapChildrenExpr (`addEnvParameterExpr` m) e

addEnvParameters :: CStatement -> FreeVars -> CStatement
addEnvParameters s m = mapChildrenStmt (`addEnvParameters` m) (`addEnvParameterExpr` m) s

-- (3) keep looping make closure factories
-- after we made the first ones and applied closures some funs return closures now
-- so we need to handle them until nothing changes
applyClosuresPasses :: CStatement -> FreeVars -> ParentMap -> Int -> CStatement
applyClosuresPasses body freeVarsMap parents freshCounter =
    let (body', closureFuns) = runState (makeClosureFactories body freeVarsMap parents) Map.empty
    in  if Map.null closureFuns then body'
        else
            let body''' = evalState (applyClosures body' body' closureFuns) freshCounter
            in applyClosuresPasses body''' freeVarsMap parents freshCounter

-- (3a) functions which immediately return another function need to return closures instead
-- if the function just returns a var, lookup the var in the map
    -- if its parent parameters contain the current params, the current function is its parent
        -- so it needs to be a closure
-- returns map of funid to the fun it makes a closure for
-- returns set of ids of parameter vars that also become closures (can't be in the same set as the functions because they are closures not functions that return closures
    -- so they can;t have the call statement)
makeClosureFactories ::  CStatement -> FreeVars -> ParentMap -> State ClosureFuns CStatement
makeClosureFactories (DefFun tret ifun params body) _ _
    | tret == CTClosure = return (DefFun tret ifun params body) -- for the looping so that it eventually terminates
makeClosureFactories (DefFun tret ifun params body) m parents =
    let funRet = findReturn body
        retId = case funRet of (Var _ i) -> i; (Val (ClosureV i)) -> i; _ -> (-1)
        returnsClosure = case funRet of (Val (ClosureV _)) -> True; _ -> False
    in  -- returns a lifted function (must be made a closure to capture env) and current function is its parent
        if Map.member retId m && isParentOf retId ifun parents then
            let allocEnv = allocateEnvironment retId ifun m params
                -- if it already returns a closure we don't need to reallocate it
                allocClos = if not returnsClosure then AllocClosure retId else Skip
                newBody = Seq allocEnv (Seq allocClos (replaceReturnClosure body retId))
            in DefFun CTClosure ifun params newBody <$ modify (Map.insert ifun retId)
        else return (DefFun tret ifun params body)
makeClosureFactories (Seq x y) m parents = Seq <$> makeClosureFactories x m parents <*> makeClosureFactories y m parents
makeClosureFactories x _ _ = return x

getClosureFnType :: CStatement -> Int -> ClosureFuns -> CType
getClosureFnType stmt innerFun closureFuns = 
    fromMaybe (CTPtr CTVoid) (getFunType stmt (followClosureIFun innerFun closureFuns))

-- (3b) change calls to closures to applications
-- the callexpr needs to be with the number of args it actually has
-- once we have the closure we can apply it with the remaining arguments
    -- I define a variable that gets sequenced before the call which holds the closure
    -- so that it doesn't need to be computed many times
-- If a closure function is passed as an argument it needs a closure allocation -> (Var _ i) case
applyClosuresExpr :: CExpression -> CStatement -> ClosureFuns -> State Int (CStatement, CExpression)
applyClosuresExpr (Var t i) _ closureFuns =
    if Map.member i closureFuns
    then return (AllocClosure i, Val (ClosureV i))
    else return (Skip, Var t i)
applyClosuresExpr (CallExpr tf tx f x) stmt closureFuns =
    let (f', args) = CDefs.collectArgs (CallExpr tf tx f x)
        fId = case f' of Var _ i -> i ; _ -> -1
    in case Map.lookup fId closureFuns of
        Just innerFun -> do -- the called function returns a closure
            let newType = getClosureFnType stmt innerFun closureFuns
            let (currArgs, otherArgs) = splitAt (getNumArgs fId stmt) args -- direct args are called, others are applied
            (pre, currArgs') <- applyClosuresArgs currArgs stmt closureFuns
            (pre', otherArgs') <- applyClosuresArgs otherArgs stmt closureFuns
            let closVar = DefVar CTClosure fId (rebuildCall tf f' currArgs')
            (castStmt, castExpr) <- applyWithCast newType (Val (ClosureV fId)) otherArgs'
            return (Seq pre (Seq pre' (Seq closVar castStmt)), castExpr)
        _ -> -- it does not return a closure
            applyClosurePre2 (CallExpr tf tx) f x stmt closureFuns
applyClosuresExpr (Ternary t c x y) stmt closureFuns = do
    (pre, c') <- applyClosuresExpr c stmt closureFuns
    (pre', x') <- applyClosuresExpr x stmt closureFuns
    (pre'', y') <- applyClosuresExpr y stmt closureFuns
    return (Seq pre (Seq pre' pre''), Ternary t c' x' y')
applyClosuresExpr (LIntOp op x y) stmt c = applyClosurePre2 (LIntOp op) x y stmt c
applyClosuresExpr (LCmpOp op x y) stmt c = applyClosurePre2 (LCmpOp op) x y stmt c
applyClosuresExpr (LBoolOp op x y) stmt c = applyClosurePre2 (LBoolOp op) x y stmt c
applyClosuresExpr (ConsList t x y) stmt c = applyClosurePre2 (ConsList t) x y stmt c
applyClosuresExpr (Not x) stmt c = applyClosurePre1 Not x stmt c
applyClosuresExpr (Abs x) stmt c = applyClosurePre1 Abs x stmt c
applyClosuresExpr (Box t x) stmt c = applyClosurePre1 (Box t) x stmt c
applyClosuresExpr (Unbox t x) stmt c = applyClosurePre1 (Unbox t) x stmt c
applyClosuresExpr (CastExpr t x) stmt c = applyClosurePre1 (CastExpr t) x stmt c
applyClosuresExpr (TailList t x) stmt c = applyClosurePre1 (TailList t) x stmt c
applyClosuresExpr (HeadList t x) stmt c = applyClosurePre1 (HeadList t) x stmt c
applyClosuresExpr (IsEmpty t x) stmt c = applyClosurePre1 (IsEmpty t) x stmt c
applyClosuresExpr (Fst t1 t2 x) stmt c = applyClosurePre1 (Fst t1 t2) x stmt c
applyClosuresExpr (Snd t1 t2 x) stmt c = applyClosurePre1 (Snd t1 t2) x stmt c
applyClosuresExpr (IndexList t x y) stmt c = applyClosurePre2 (IndexList t) x y stmt c
applyClosuresExpr (Prod t x y) stmt c = applyClosurePre2 (Prod t) x y stmt c
applyClosuresExpr (ApplyClosure t x y) stmt c = applyClosurePre2 (ApplyClosure t) x y stmt c
applyClosuresExpr x _ _ = return (Skip, x)

applyClosurePre1 :: (CExpression -> CExpression) -> CExpression -> CStatement -> ClosureFuns -> State Int (CStatement, CExpression)
applyClosurePre1 f x stmt c = do 
    (pre, x') <- applyClosuresExpr x stmt c
    return (pre, f x')

applyClosurePre2 :: (CExpression -> CExpression -> CExpression) -> CExpression -> CExpression -> CStatement -> ClosureFuns -> State Int (CStatement, CExpression)
applyClosurePre2 f x y stmt c = do 
    (pre, x') <- applyClosuresExpr x stmt c
    (pre', y') <- applyClosuresExpr y stmt c
    return (Seq pre pre', f x' y')

applyClosuresArgs :: CArgs -> CStatement -> ClosureFuns -> State Int (CStatement, CArgs)
applyClosuresArgs [] _ _ = return (Skip, [])
applyClosuresArgs [CArg t x] stmt closureFuns = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns
    return (pre, [CArg t x'])
applyClosuresArgs (arg : rest) stmt closureFuns = do
    (pre, arg') <- applyClosuresArgs [arg] stmt closureFuns
    (pre', rest') <- applyClosuresArgs rest stmt closureFuns
    return (Seq pre pre', arg' ++ rest')

applyClosures :: CStatement -> CStatement -> ClosureFuns -> State Int CStatement
applyClosures (DefFun tret ifun params body) stmt closureFuns = 
    DefFun tret ifun params <$> applyClosures body stmt closureFuns
applyClosures (Seq x y) stmt closureFuns = 
    Seq <$> applyClosures x stmt closureFuns <*> applyClosures y stmt closureFuns
applyClosures (If c x y) stmt closureFuns = do
    x' <- applyClosures x stmt closureFuns
    y' <- applyClosures y stmt closureFuns
    (pre, c') <- applyClosuresExpr c stmt closureFuns
    return $ Seq pre (If c' x' y')
applyClosures (While c x) stmt closureFuns = do
    (pre, c') <- applyClosuresExpr c stmt closureFuns
    x' <- applyClosures x stmt closureFuns
    return $ Seq pre (While c' x')
applyClosures (Return x) stmt closureFuns = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns
    return $ Seq pre (Return x')
applyClosures (DefVar t i x) stmt closureFuns = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns
    return $ Seq pre (DefVar t i x')
applyClosures (UpdateVar t i x) stmt closureFuns = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns
    return $ Seq pre (UpdateVar t i x')
applyClosures x _ _ = return x

-- (4) add env  allocations for all the functions we call in the body of this function
-- we can call a function several times so we shouldn't redefine the same env (only depends on our param)
-- all functions that were lifted need an env alloc
-- env alloc only needs the current param if this function its its parent
    -- don't add duplicates, so not if its already alloced, or in the current params
addEnvAllocs :: CStatement -> CStatement -> FreeVars -> CStatement
addEnvAllocs (DefFun tret ifun params body) stmt freeVarsMap =
    let funInfo = getFunctionInfo body emptyFunctionInfo
        closureFunDefs = getClosureDefs stmt
        usedFunVars = Set.fromList (Map.keys (varUses funInfo) `intersect` closureFunDefs)
        calledFuns = Set.fromList (Map.keys (functionCalls funInfo))
        allUsedFuns = Set.toList (Set.union calledFuns usedFunVars) \\ (Set.toList (allocedEnvs funInfo) ++ getEnvParams params)
        allocs = foldr (Seq . (\i -> allocateEnvironment i ifun freeVarsMap params)) Skip allUsedFuns
    in DefFun tret ifun params (Seq allocs body)
addEnvAllocs (Seq x y) stmt liftedFuns =
    Seq (addEnvAllocs x stmt liftedFuns) (addEnvAllocs y stmt liftedFuns)
addEnvAllocs x _ _ = x

allocateEnvironment :: Int -> Int -> FreeVars -> CParams -> CStatement
allocateEnvironment i ifun freeVarsMap params =
    case Map.lookup i freeVarsMap of
        Just freeVars ->
            let parentParams = (Set.toList freeVars \\ params)
                directParams = Set.toList freeVars \\ parentParams
                parentParams' = paramsToArgGetEnv parentParams ifun
                directParams' = paramsToArgVars directParams
            in  AllocEnv i ifun (Map.union directParams' parentParams')
        _ -> Skip

-- follow the id of the current function in the parent map so see if the first int is the parent of the second
isParentOf :: Int -> Int -> ParentMap -> Bool
isParentOf child parent parentMap =
    case Map.lookup child parentMap of
        Just i
            | i == parent -> True
            | otherwise -> isParentOf i parent parentMap
        Nothing -> False

paramsToArgVars :: CParams -> CArgMap
paramsToArgVars [] = Map.empty
paramsToArgVars [CParam i t] = Map.singleton i (CArg t (Var t i))
paramsToArgVars [CParamEnv i] = Map.singleton i (CArg (CTPtr CTVoid) (Val (EnvV i)))
paramsToArgVars (i:is) = Map.union (paramsToArgVars [i]) (paramsToArgVars is)

paramsToArgGetEnv :: CParams -> Int -> CArgMap
paramsToArgGetEnv [] _ = Map.empty
paramsToArgGetEnv [CParam i t] parent = Map.singleton i (CArg t (GetEnvField t parent i))
paramsToArgGetEnv [CParamEnv i] parent = Map.singleton i (CArg (CTPtr CTVoid) (GetEnvField (CTPtr CTVoid) parent i))
paramsToArgGetEnv (i:is) parent = Map.union (paramsToArgGetEnv [i] parent) (paramsToArgGetEnv is parent)

------ (5) Pass to add box/unbox

addBoxing :: CStatement -> CStatement
addBoxing = mapChildrenStmt addBoxing addBoxingExpr

addBoxingExpr :: CExpression -> CExpression
addBoxingExpr (ApplyClosure tx f x) = ApplyClosure tx (addBoxingExpr f) (Box tx (addBoxingExpr x))
addBoxingExpr e = mapChildrenExpr addBoxingExpr e

-- ─────────────────────────────────────────────
--  Lambda Lifting Helpers
-- ─────────────────────────────────────────────

followClosureIFun :: Int -> ClosureFuns -> Int
followClosureIFun i m =
    case Map.lookup i m of
        Just next -> followClosureIFun next m
        _ -> i

applyWithCast :: CType -> CExpression -> CArgs -> State Int (CStatement, CExpression)
applyWithCast _ base [] = return (Skip, base)
applyWithCast retType base [CArg t a] = return (Skip, CastExpr retType (ApplyClosure t base a))
applyWithCast retType base (CArg t a : rest) = do
    closId <- fresh
    let closVar = DefVar CTClosure closId (ApplyClosure t base a)
    (innerStmt, finalExpr) <- applyWithCast retType (Val (ClosureV closId)) rest
    return (Seq closVar innerStmt, finalExpr)

findReturn :: CStatement -> CExpression
findReturn (Return x) = x
findReturn (Seq _ y) = findReturn y
findReturn (If _ x _) = findReturn x
findReturn (While _ x) = findReturn x
findReturn _ = error "no return"

replaceReturnClosure :: CStatement -> Int -> CStatement
replaceReturnClosure (Return _) i = Return (Val (ClosureV i))
replaceReturnClosure s i = mapChildrenStmt (`replaceReturnClosure` i) id s

rebuildCall :: CType -> CExpression -> CArgs -> CExpression
rebuildCall tf = foldl (\acc (CArg ta a) -> CallExpr tf ta acc a)

findFirstDefFun :: CStatement -> Maybe CStatement
findFirstDefFun stmt@DefFun{} = Just stmt
findFirstDefFun (Seq x y) =
    let xdef = findFirstDefFun x
    in case xdef of
        Nothing -> findFirstDefFun y
        _ -> xdef
findFirstDefFun (If _ x y) =
    let xdef = findFirstDefFun x
    in case xdef of
        Nothing -> findFirstDefFun y
        _ -> xdef
findFirstDefFun _ = Nothing

removeDefFun :: CStatement -> Int -> CStatement
removeDefFun (DefFun tret ifun' params body) ifun
    | ifun == ifun' = Skip
    | otherwise = DefFun tret ifun' params (removeDefFun body ifun)
removeDefFun s ifun = mapChildrenStmt (`removeDefFun` ifun) id s

getNumArgs :: Int -> CStatement -> Int
getNumArgs fId stmt =
    let mergedMap = Map.map length (getFunsWithParams stmt)
    in Map.findWithDefault 1 fId mergedMap

-- ─────────────────────────────────────────────
--  Generate Env Structs and Pair Defs
-- ─────────────────────────────────────────────

genPairDeclaration :: (CType, CType) -> String
genPairDeclaration (a, b) =
    let strA = printType a
        strB = printType b
        strAB = printPairType a ++ "_" ++ printPairType b
        pairType = "Pair_" ++ strAB
    in
        "\ntypedef struct " ++ pairType ++ " {"
    ++ "\n  " ++ strA ++ " fst;"
    ++ "\n  " ++ strB ++ " snd;"
    ++ "\n} " ++ pairType ++ ";"
    ++ "\n\n" ++ pairType ++ "* make" ++ pairType ++ "(" ++ strA ++ " fst, " ++ strB ++ " snd) {"
    ++ "\n  " ++ pairType ++ "* p = malloc(sizeof(" ++ pairType ++ "));"
    ++ "\n  p->fst = fst;\n  p->snd = snd;\n  return p;"
    ++ "\n};\n"

genEnvStructs :: Int -> FreeVars -> CStatement
genEnvStructs ifun freeVars =
    let envParams = maybe [] Set.toList (Map.lookup ifun freeVars)
    in DefEnvStruct ifun envParams

-- ─────────────────────────────────────────────
--  Main 
-- ─────────────────────────────────────────────

removeFirstReturn :: CStatement -> CStatement
removeFirstReturn (Return _) = Skip
removeFirstReturn (Seq x y) = Seq x (removeFirstReturn y)
removeFirstReturn (If c t e) = If c (removeFirstReturn t) (removeFirstReturn e)
removeFirstReturn (While c x) = While c (removeFirstReturn x)
removeFirstReturn x = x

splitTopLevel :: CStatement -> (CStatement, CStatement)
splitTopLevel (Seq l@DefFun{} y) =
    let (funs, body) = splitTopLevel y
    in (Seq l funs, body)
splitTopLevel (Seq l@DefEnvStruct{} y) =
    let (funs, body) = splitTopLevel y
    in (Seq l funs, body)
splitTopLevel (Seq Skip y) = splitTopLevel y
splitTopLevel (Seq x y) =
    let (funs, body) = splitTopLevel x
        (funs', body') = splitTopLevel y
    in (Seq funs funs', Seq body body')
splitTopLevel Skip = (Skip, Skip)
splitTopLevel l@DefFun{} = (l, Skip)
splitTopLevel x = (Skip, x)

basicCompile :: Typeable a => AL.Lang a -> (CStatement, Int)
basicCompile progCode =
    let (nl, fresh') = runState (NL.translate progCode) 0
        (clBase, fresh'') = runState (CL.translate nl) fresh'
        clOpt = CL.optimizeBindings clBase
        c = translate clOpt
    in (c, fresh'')

liftLambdasAndMerge :: Bool -> CStatement -> Int -> (CStatement, FreeVars)
liftLambdasAndMerge canMerge body freshInt =
    let body0 = if canMerge then mergeLambdas body body else body
        (body1, (freeVarsMap, parentMap)) = runState (lambdaLift body0) (Map.empty, Map.empty)
        body2 = addEnvParameters body1 freeVarsMap
        body3 = applyClosuresPasses body2 freeVarsMap parentMap freshInt
        body4 = addEnvAllocs body3 body3 freeVarsMap
        body5 = addBoxing body4
    in (body5, freeVarsMap)

printCCode :: CStatement -> FreeVars -> String
printCCode finalBody freeVars =
    let finalDefs = getDefs finalBody
        finalMergeMap = Map.map length (getFunsWithParams finalBody)
        globalInfo = getGlobalInfo finalBody emptyGlobalInfo

        envStructs = foldr (Seq . (`genEnvStructs` freeVars)) Skip (Set.toList (usedEnvsGlobal globalInfo))
        
        imports =   "\n#include <stdbool.h>" ++
                    "\n#include <stdio.h>" ++
                    "\n#include <stdlib.h>" ++
                    "\n#include <stdint.h>" ++
                    "\n#include \"../lib.c\"\n"
        (funPart, mainBody) = splitTopLevel finalBody
        retExpr = findReturn mainBody
        mainBodyWithoutRet = removeFirstReturn mainBody
        retImpl = showCExpression retExpr finalMergeMap
        mainBodyImpl = showCStmt 1 finalMergeMap mainBodyWithoutRet
        funImpl = showCStmt 0 finalMergeMap funPart

        in "\n// imports" ++ imports ++
            "\n// pair type defitions" ++ concatMap genPairDeclaration (Set.toList (pairTypesGlobal globalInfo)) ++
            "\n// function defitions" ++ showFunDefs finalDefs ++
            "\n\n// closure defitions" ++ showCStmt 0 Map.empty envStructs ++
            "\n// function implementations" ++ funImpl ++
            "\n// main\nint main(void) {" ++ mainBodyImpl ++
                    case getTypeExpr retExpr of
                        CTInt -> "\n  printInt("
                        CTListInt -> "\n  printListInt("
                        CTFun _ CTInt -> "\n  printInt("
                        CTFun _ CTListInt -> "\n  printListInt("
                        _ ->error "cannot print"
            ++ retImpl ++ ");\n" ++ "  return 0;\n}\n"
