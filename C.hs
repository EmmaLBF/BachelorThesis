{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
import Debug.Trace
import Unsafe.Coerce
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )

-- translate

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

translateExpr :: forall a. CL.CExpression a -> CExpression a
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
translateExpr (CL.IndexList (l :: CL.CExpression [b]) i) = IndexList (fromTypeRep (typeRep (Proxy :: Proxy b))) (translateExpr l) (translateExpr i)
translateExpr (CL.LIntOp op e1 e2) = LIntOp op (translateExpr e1) (translateExpr e2)
translateExpr (CL.LCmpOp op e1 e2) = LCmpOp op (translateExpr e1) (translateExpr e2)
translateExpr (CL.LBoolOp op e1 e2) = LBoolOp op (translateExpr e1) (translateExpr e2)
translateExpr (CL.Ternary c t f) = Ternary (fromTypeRep (typeRep (Proxy :: Proxy a))) (translateExpr c) (translateExpr t) (translateExpr f)

translate :: CL.CStatement a -> CStatement a
translate CL.Skip = Skip
translate (CL.Return x) = Return (translateExpr x)
translate (CL.DefVar i (x :: CL.CExpression a)) =
    DefVar (fromTypeRep (typeRep (Proxy :: Proxy a))) i (translateExpr x)
translate (CL.UpdateVar i (x :: CL.CExpression a)) =
    UpdateVar (fromTypeRep (typeRep (Proxy :: Proxy a))) i (translateExpr x)
translate (CL.While cond x) = While (translateExpr cond) (translate x)
translate (CL.Seq x y) = Seq (translate x) (translate y)
translate (CL.BindExpr (x :: CL.CExpression a) i s) =
    BindExpr (fromTypeRep (typeRep (Proxy :: Proxy a))) (translateExpr x) i (translate s)
translate (CL.If cond x y) = If (translateExpr cond) (translate x) (translate y)
translate (CL.DefFun tret ifun (ip, tp) body) =
    DefFun (fromTypeRep (typeRep tret)) ifun [CParam ip (fromTypeRep (typeRep tp))] (translate body)

------ Pass to add box/unbox

addBoxing :: CStatement a -> CStatement a
addBoxing (DefFun tret ifun params body) = DefFun tret ifun params (addBoxing body)
addBoxing (Seq x y) = Seq (addBoxing x) (addBoxing y)
addBoxing (Return x) = Return (addBoxingExpr x)
addBoxing (BindExpr t x i y) = BindExpr t (addBoxingExpr x) i (addBoxing y)
addBoxing (If c x y) = If (addBoxingExpr c) (addBoxing x) (addBoxing y)
addBoxing (While c x) = While (addBoxingExpr c) (addBoxing x)
addBoxing (DefVar t i x) = DefVar t i (addBoxingExpr x)
addBoxing (UpdateVar t i x) = UpdateVar t i (addBoxingExpr x)
addBoxing x = x

addBoxingExpr :: CExpression a -> CExpression a
addBoxingExpr (HeadList t x) = HeadList t (addBoxingExpr x)
addBoxingExpr (Fst tp tr x) = Fst tp tr (addBoxingExpr x)
addBoxingExpr (Snd tp tr x) = Snd tp tr (addBoxingExpr x)
addBoxingExpr (ConsList t x y) = ConsList t (addBoxingExpr x) (addBoxingExpr y)
addBoxingExpr (ApplyClosure tx f x) = ApplyClosure tx (addBoxingExpr f) (Box tx (addBoxingExpr x))
addBoxingExpr (LIntOp op x y) = LIntOp op (addBoxingExpr x) (addBoxingExpr y)
addBoxingExpr (LCmpOp op x y) = LCmpOp op (addBoxingExpr x) (addBoxingExpr y)
addBoxingExpr (LBoolOp op x y) = LBoolOp op (addBoxingExpr x) (addBoxingExpr y)
addBoxingExpr (Ternary tp c t e) = Ternary tp (addBoxingExpr c) (addBoxingExpr t) (addBoxingExpr e)
addBoxingExpr (Not x) = Not (addBoxingExpr x)
addBoxingExpr (Abs x) = Abs (addBoxingExpr x)
addBoxingExpr (IsEmpty t x) = IsEmpty t (addBoxingExpr x)
addBoxingExpr (CastExpr t x) = CastExpr t (addBoxingExpr x)
addBoxingExpr (CallExpr tf tx f x) = CallExpr tf tx (addBoxingExpr f) (addBoxingExpr x)
addBoxingExpr (TailList t x) = TailList t (addBoxingExpr x)
addBoxingExpr (IndexList t i x) = IndexList t i (addBoxingExpr x)
addBoxingExpr (Prod t x y) = Prod t (addBoxingExpr x) (addBoxingExpr y)
addBoxingExpr x = x

-- LAMBDA LIFTING

--------- FREE VARS

-- free, bound
freeVarsExpr :: CExpression a -> (CParamMap, CParamMap)
freeVarsExpr (Not x) = freeVarsExpr x
freeVarsExpr (Abs x) = freeVarsExpr x
freeVarsExpr (Fst _ _ x) = freeVarsExpr x
freeVarsExpr (Snd _ _ x) = freeVarsExpr x
freeVarsExpr (Box _ x) = freeVarsExpr x
freeVarsExpr (Unbox _ x) = freeVarsExpr x
freeVarsExpr (IsEmpty _ l) = freeVarsExpr l
freeVarsExpr (TailList _ l) = freeVarsExpr l
freeVarsExpr (HeadList _ l) = freeVarsExpr l
freeVarsExpr (IndexList _ l _) = freeVarsExpr l
freeVarsExpr (Prod _ x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (LIntOp _ x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (LCmpOp _ x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (LBoolOp _ x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (CallExpr _ _ f x) = merge (freeVarsExpr f) (freeVarsExpr x)
freeVarsExpr (ConsList _ l x) = merge (freeVarsExpr l) (freeVarsExpr x)
freeVarsExpr (Var t i) = (Map.singleton i (CParam i t), Map.empty)
freeVarsExpr (Ternary _ cond thn els) = merge (merge (freeVarsExpr cond) (freeVarsExpr thn)) (freeVarsExpr els)
freeVarsExpr _ = (Map.empty, Map.empty)

freeVarsStmt :: CStatement a -> (CParamMap, CParamMap)
freeVarsStmt (BindExpr t x i y) =
    let (mfree, mbound) = merge (freeVarsExpr x) (freeVarsStmt y)
    in (mfree, Map.insert i (CParam i t) mbound)
freeVarsStmt (Seq x y) = merge (freeVarsStmt x) (freeVarsStmt y)
freeVarsStmt (If cond x y) = merge (freeVarsExpr cond) (merge (freeVarsStmt x) (freeVarsStmt y))
freeVarsStmt (While cond x) = merge (freeVarsExpr cond) (freeVarsStmt x)
freeVarsStmt (DefFun _ ifun params body) =
    let (bfree, bbound) = freeVarsStmt body
        boundKeys = (Map.fromList . Prelude.map (\p -> (paramId p, p))) params
        locallyBound = Map.insert ifun undefined boundKeys
        actualFree = Map.difference bfree locallyBound
    in (actualFree, Map.insert ifun undefined (Map.union bbound boundKeys))
freeVarsStmt (UpdateVar t i x) =
    let (xfree, xbound) = freeVarsExpr x
    in (Map.union (Map.singleton i (CParam i t)) xfree, xbound)
freeVarsStmt (DefVar t i x) =
    let (xfree, xbound) = freeVarsExpr x
    in (xfree, Map.insert i (CParam i t) xbound)
freeVarsStmt (Return x) = freeVarsExpr x
freeVarsStmt _ = (Map.empty, Map.empty)

freeVars :: CStatement a -> CParamMap
freeVars s =
    let (free, bound) = freeVarsStmt s
    in Map.difference free bound

--------- HOISTING HELPERS

findReturn :: CStatement a -> Maybe (CExpression a)
findReturn (Return x) = Just x
findReturn (BindExpr _ _ _ y) = findReturn y
findReturn (Seq _ y) = findReturn y
findReturn (If _ x _) = findReturn x
findReturn (While _ x) = findReturn x
findReturn _ = Nothing

replaceReturnClosure :: CStatement a -> Int -> CStatement a
replaceReturnClosure (Return _) i = Return (Val (ClosureV i))
replaceReturnClosure (BindExpr t x c y) i = BindExpr t x c (replaceReturnClosure y i)
replaceReturnClosure (Seq x y) i = Seq (replaceReturnClosure x i) (replaceReturnClosure y i)
replaceReturnClosure (If c x y) i = If c (replaceReturnClosure x i) (replaceReturnClosure y i)
replaceReturnClosure (While c y) i = While c (replaceReturnClosure y i)
replaceReturnClosure x _ = x

rebuildCall :: CType -> CExpression a -> [CArg] -> CExpression a
rebuildCall tf = foldl (\acc (CArg ta a) -> CallExpr tf ta (unsafeCoerce acc) a)

findFirstDefFun :: CStatement a -> Maybe (CStatement a)
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

removeDefFun :: CStatement a -> Int -> CStatement a
removeDefFun (DefFun tret ifun' params body) ifun
    | ifun == ifun' = Skip
    | otherwise = DefFun tret ifun' params (removeDefFun body ifun)
removeDefFun (Seq x y) ifun = Seq (removeDefFun x ifun) (removeDefFun y ifun)
removeDefFun (If c x y) ifun = If c (removeDefFun x ifun) (removeDefFun y ifun)
removeDefFun (While c x) ifun = While c (removeDefFun x ifun)
removeDefFun x _ = x

--------- HOISTING

-- for each def search in its body for the first def you find
-- if there is one we lift it out (sequence it before) and remove it from the body
-- the lifted function needs an env as a parameter with the params of all its parents
-- this only lifts one def at a time

-- also returns map of which function is which parent
liftDefs :: CStatement a -> State (ParentParams, Map.Map Int Int) (CStatement a)
liftDefs stmt@(DefFun tret ifun params body) =
    let defToRemove = findFirstDefFun body -- find a def nested inside the current one
    in case defToRemove of
        Nothing -> return stmt -- no more defs to lift out
        Just (DefFun tret' ifun' params' body') -> do
            let removedDefBody = removeDefFun body ifun' -- remove the def we found
                newDef = DefFun tret' ifun' (CParamEnv ifun' : params') body' -- add an env param to it and lift it out
                usedInNested = Map.keysSet (varUses (getFunctionInfo body' emptyFunctionInfo)) -- vars that are used in the nested body
            modify $ \(m, n) ->
                let parentVars = Set.union (Set.fromList params) (Map.findWithDefault Set.empty ifun m)
                    neededVars = Set.filter (\p -> Set.member (paramId p) usedInNested) parentVars
                in (Map.insert ifun' neededVars m, Map.insert ifun' ifun n) -- add all of its needed params to the map
            return (Seq newDef (DefFun tret ifun params removedDefBody))
        _ -> error "not def"
liftDefs (Seq x y) = do
    m <- get
    x' <- liftDefs x
    m' <- get
    if m' /= m
        then return (Seq x' y)
        else Seq x <$> liftDefs y
liftDefs x = return x

lambdaLift :: CStatement a -> State (ParentParams, Map.Map Int Int) (CStatement a)
lambdaLift stmt = do
    (m, _) <- get
    stmt' <- liftDefs stmt
    (m', _) <- get
    let stmt'' = replaceParentVarAccess stmt' (-1) m'
    if m' /= m then lambdaLift stmt'' else return stmt''

-- all of the parent function(s)'s paramteters need to be accessed through the env
-- instead of directly through var
replaceParentVarAccessExpr :: CExpression a -> Int -> ParentParams -> CExpression a
replaceParentVarAccessExpr (Var t i) currFun m =
    case Map.lookup currFun m of
        Just funSet | i `elem` paramsToList (Set.toList funSet) -> GetEnvField t currFun i
        _ -> Var t i
replaceParentVarAccessExpr (LIntOp op x y) currFun m = LIntOp op (replaceParentVarAccessExpr x currFun m) (replaceParentVarAccessExpr y currFun m)
replaceParentVarAccessExpr (LCmpOp op x y) currFun m = LCmpOp op (replaceParentVarAccessExpr x currFun m) (replaceParentVarAccessExpr y currFun m)
replaceParentVarAccessExpr (LBoolOp op x y) currFun m = LBoolOp op (replaceParentVarAccessExpr x currFun m) (replaceParentVarAccessExpr y currFun m)
replaceParentVarAccessExpr (ConsList t x y) currFun m = ConsList t (replaceParentVarAccessExpr x currFun m) (replaceParentVarAccessExpr y currFun m)
replaceParentVarAccessExpr (CallExpr tx ty x y) currFun m = CallExpr tx ty (replaceParentVarAccessExpr x currFun m) (replaceParentVarAccessExpr y currFun m)
replaceParentVarAccessExpr (Not x) currFun m = Not (replaceParentVarAccessExpr x currFun m)
replaceParentVarAccessExpr (Abs x) currFun m = Abs (replaceParentVarAccessExpr x currFun m)
replaceParentVarAccessExpr (TailList t x) currFun m = TailList t (replaceParentVarAccessExpr x currFun m)
replaceParentVarAccessExpr (HeadList t x) currFun m = HeadList t (replaceParentVarAccessExpr x currFun m)
replaceParentVarAccessExpr (IsEmpty t x) currFun m = IsEmpty t (replaceParentVarAccessExpr x currFun m)
replaceParentVarAccessExpr (Fst t1 t2 x) currFun m = Fst t1 t2 (replaceParentVarAccessExpr x currFun m)
replaceParentVarAccessExpr (Snd t1 t2 x) currFun m = Snd t1 t2 (replaceParentVarAccessExpr x currFun m)
replaceParentVarAccessExpr (IndexList t x y) currFun m = IndexList t (replaceParentVarAccessExpr x currFun m) (replaceParentVarAccessExpr y currFun m)
replaceParentVarAccessExpr (Prod t x y) currFun m = Prod t (replaceParentVarAccessExpr x currFun m) (replaceParentVarAccessExpr y currFun m)
replaceParentVarAccessExpr x _ _ = x

replaceParentVarAccess :: CStatement a -> Int -> ParentParams -> CStatement a
replaceParentVarAccess (UpdateVar t i x) currFun m = UpdateVar t i (replaceParentVarAccessExpr x currFun m)
replaceParentVarAccess (DefVar t i x) currFun m = DefVar t i (replaceParentVarAccessExpr x currFun m)
replaceParentVarAccess (Seq x y) currFun m = Seq (replaceParentVarAccess x currFun m) (replaceParentVarAccess y currFun m)
replaceParentVarAccess (If c x y) currFun m = If (replaceParentVarAccessExpr c currFun m) (replaceParentVarAccess x currFun m) (replaceParentVarAccess y currFun m)
replaceParentVarAccess (While c y) currFun m = While (replaceParentVarAccessExpr c currFun m) (replaceParentVarAccess y currFun m)
replaceParentVarAccess (BindExpr t c i y) currFun m = BindExpr t (replaceParentVarAccessExpr c currFun m) i (replaceParentVarAccess y currFun m)
replaceParentVarAccess (DefFun t ifun p body) _ m = DefFun t ifun p (replaceParentVarAccess body ifun m)
replaceParentVarAccess (Return c) currFun m = Return (replaceParentVarAccessExpr c currFun m)
replaceParentVarAccess x _ _ = x


isParentOf :: Int -> Int -> Map.Map Int Int -> Bool
isParentOf child parent parentMap =
    case Map.lookup child parentMap of
        Just i
            | i == parent -> True
            | otherwise -> isParentOf i parent parentMap
        Nothing -> False

paramsToArgVars :: CParams -> Map.Map Int CArg
paramsToArgVars [] = Map.empty
paramsToArgVars [CParam i t] = Map.singleton i (CArg t (Var t i))
paramsToArgVars [CParamEnv i] = Map.singleton i (CArg CTVoidPtr (Val (EnvV i)))
paramsToArgVars (i:is) = Map.union (paramsToArgVars [i]) (paramsToArgVars is)

paramsToArgGetEnv :: CParams -> Int -> Map.Map Int CArg
paramsToArgGetEnv [] _ = Map.empty
paramsToArgGetEnv [CParam i t] parent = Map.singleton i (CArg t (GetEnvField t parent i))
paramsToArgGetEnv [CParamEnv i] parent = Map.singleton i (CArg CTVoidPtr (GetEnvField CTVoidPtr parent i))
paramsToArgGetEnv (i:is) parent = Map.union (paramsToArgGetEnv [i] parent) (paramsToArgGetEnv is parent)

-- functions which immediately return another function need to return closures instead
-- if the function just returns a var, lookup the var in the map
    -- if its parent parameters contain the current params, the current function is its parent
        -- so it needs to be a closure
-- returns map of funid to the fun it makes a closure for
-- returns set of ids of parameter vars that also become closures (can't be in the same set as the functions because they are closures not functions that return closures
    -- so they can;t have the call statement)
makeClosureFactories :: CStatement a -> ParentParams -> Map.Map Int Int -> ClosureFuns -> (CStatement a, ClosureFuns)
makeClosureFactories (DefFun tret ifun params body) m parents closures =
    let funRet = findReturn body
        retId = case funRet of
            Just (Var _ i) -> i
            Just (Val (ClosureV i)) -> i
            _ -> (-1)
        returnsClosure = case funRet of
            Just (Val (ClosureV _)) -> True
            _ -> False
    in  if tret == CTClosure
        then (DefFun tret ifun params body, closures) -- for the looping so that it eventually terminates
        else case Map.lookup retId m of
                Just funSet -- returns a lifted function (must be made a closure to capture env)
                    | isParentOf retId ifun parents -> -- curr fun is parent
                        let parentParams = Set.toList funSet \\ params
                            directParams = Set.toList (Set.intersection (Set.fromList params) funSet)
                            allocEnv = AllocEnv retId ifun (paramsToArgVars directParams) (paramsToArgGetEnv parentParams ifun)
                            allocCls = if not returnsClosure then AllocClosure retId else Skip -- if its retun is alsready a closure we don't need to reallocate it
                            newBody = Seq allocEnv (Seq allocCls (replaceReturnClosure body retId))
                        in (DefFun CTClosure ifun params newBody, Map.insert ifun retId closures)
                _ -> (DefFun tret ifun params body, closures)
makeClosureFactories (Seq x y) m parents closures =
    let (x', c) = makeClosureFactories x m parents closures
        (y', c') = makeClosureFactories y m parents closures
    in (Seq x' y', Map.union c c')
makeClosureFactories x _ _ closures = (x, closures)

-- add env parameters to call sites of hoisted functions
-- if we call a function that is in our lifted set we need to make an env to its call list
addEnvParameterExpr :: CExpression a -> ParentParams -> CExpression a
addEnvParameterExpr (CallExpr tf tx f x) m =
    let (f', args) = CDefs.collectArgs (CallExpr tf tx f x)
    in case f' of
        (Var _ i) ->
            case Map.lookup i m of
                Just _ -> rebuildCall tf f' (CArg CTVoidPtr (Val (EnvV i)) : map (\(CArg t arg)-> CArg t (addEnvParameterExpr arg m)) args)
                _ -> CallExpr tf tx (addEnvParameterExpr f m) (addEnvParameterExpr x m)
        _ -> CallExpr tf tx (addEnvParameterExpr f m) (addEnvParameterExpr x m)
addEnvParameterExpr (LIntOp op x y) m = LIntOp op (addEnvParameterExpr x m) (addEnvParameterExpr y m)
addEnvParameterExpr (LCmpOp op x y) m = LCmpOp op (addEnvParameterExpr x m) (addEnvParameterExpr y m)
addEnvParameterExpr (LBoolOp op x y) m = LBoolOp op (addEnvParameterExpr x m) (addEnvParameterExpr y m)
addEnvParameterExpr (ConsList t x y) m = ConsList t (addEnvParameterExpr x m) (addEnvParameterExpr y m)
addEnvParameterExpr (Not x) m = Not (addEnvParameterExpr x m)
addEnvParameterExpr (Abs x) m = Abs (addEnvParameterExpr x m)
addEnvParameterExpr (Box t x) m = Box t (addEnvParameterExpr x m)
addEnvParameterExpr (Unbox t x) m = Unbox t (addEnvParameterExpr x m)
addEnvParameterExpr (CastExpr t x) m = CastExpr t (addEnvParameterExpr x m)
addEnvParameterExpr (TailList t x) m = TailList t (addEnvParameterExpr x m)
addEnvParameterExpr (HeadList t x) m = HeadList t (addEnvParameterExpr x m)
addEnvParameterExpr (IsEmpty t x) m = IsEmpty t (addEnvParameterExpr x m)
addEnvParameterExpr (Fst t1 t2 x) m = Fst t1 t2 (addEnvParameterExpr x m)
addEnvParameterExpr (Snd t1 t2 x) m = Snd t1 t2 (addEnvParameterExpr x m)
addEnvParameterExpr (IndexList t x y) m = IndexList t (addEnvParameterExpr x m) (addEnvParameterExpr y m)
addEnvParameterExpr (Prod t x y) m = Prod t (addEnvParameterExpr x m) (addEnvParameterExpr y m)
addEnvParameterExpr (Ternary t c x y) m = Ternary t (addEnvParameterExpr c m) (addEnvParameterExpr x m) (addEnvParameterExpr y m)
addEnvParameterExpr (Val x) _ = Val x
addEnvParameterExpr (EmptyList x) _ = EmptyList x
addEnvParameterExpr (Var t x) _ = Var t x
addEnvParameterExpr (GetEnvField t x i) _ = GetEnvField t x i
addEnvParameterExpr x _ = x

addEnvParameter :: CStatement a -> ParentParams -> CStatement a
addEnvParameter (UpdateVar t i x) m = UpdateVar t i (addEnvParameterExpr x m)
addEnvParameter (DefVar t i x) m = DefVar t i (addEnvParameterExpr x m)
addEnvParameter (Seq x y) m = Seq (addEnvParameter x m) (addEnvParameter y m)
addEnvParameter (If c x y) m = If (addEnvParameterExpr c m) (addEnvParameter x m) (addEnvParameter y m)
addEnvParameter (While c y) m = While (addEnvParameterExpr c m) (addEnvParameter y m)
addEnvParameter (BindExpr t c i y) m = BindExpr t (addEnvParameterExpr c m) i (addEnvParameter y m)
addEnvParameter (DefFun t ifun p body) m = DefFun t ifun p (addEnvParameter body m)
addEnvParameter (Return c) m = Return (addEnvParameterExpr c m)
addEnvParameter x _ = x

getEnvParams :: CParams -> [Int]
getEnvParams [] = []
getEnvParams [CParamEnv i] = [i]
getEnvParams [_] = []
getEnvParams (i:is) = getEnvParams [i] ++ getEnvParams is

-- add env  allocations for all the functions we call in the body of this function
-- we can call a function several times so we shouldn't redefine the same env (only depends on our param)
-- all functions that were lifted need an env alloc
-- env alloc only needs the current param if this function its its parent
    -- don't add duplicates, so not if its already alloced, or in the current params
addEnvAllocs :: CStatement a -> CStatement a -> ParentParams -> CStatement a
addEnvAllocs (DefFun tret ifun params body) stmt liftedFuns =
    let funInfo = getFunctionInfo body emptyFunctionInfo
        closureFunDefs = getClosureDefs stmt
        usedFunVars = Set.fromList (intersect (Map.keys (varUses funInfo)) closureFunDefs)
        calledFuns = Set.fromList (Map.keys (functionCalls funInfo))
        allUsedFuns = Set.toList (Set.union calledFuns usedFunVars) \\ (Set.toList (allocedEnvs funInfo) ++ getEnvParams params)
        allocs = foldr Seq Skip (map allocFun allUsedFuns)
    in DefFun tret ifun params (Seq allocs body)
    where
        allocFun :: Int -> CStatement a
        allocFun i =
            case Map.lookup i liftedFuns of
                Just parentParams ->
                    let parentParams' = (Set.toList parentParams \\ params)
                        directParams = Set.toList parentParams \\ parentParams'
                    in AllocEnv i ifun (paramsToArgVars directParams) (paramsToArgGetEnv parentParams' ifun)
                _ -> Skip
addEnvAllocs (Seq x y) stmt liftedFuns =
    Seq (addEnvAllocs x stmt liftedFuns) (addEnvAllocs y stmt liftedFuns)
addEnvAllocs x _ _ = x

-- follow closure type through map
followClosureIFun :: Int -> ClosureFuns -> Int
followClosureIFun i m =
    case Map.lookup i m of
        Just next -> followClosureIFun next m
        _ -> i

-- number of hops through map depends on num of args
applyWithCast :: CType -> CExpression a -> [CArg] -> State Int (CStatement c, CExpression b)
applyWithCast _ base [] = return (Skip, unsafeCoerce base)
applyWithCast retType base [CArg t a] = return (Skip, CastExpr retType (ApplyClosure t base a))
applyWithCast retType base (CArg t a : rest) = do
    closId <- fresh
    let closVar = DefVar CTClosure closId (ApplyClosure t base a)
    (innerStmt, finalExpr) <- applyWithCast retType (Val (ClosureV closId)) rest
    return (Seq closVar innerStmt, finalExpr)

-- change calls to closures to applications
-- the callexpr needs to be with the number of args it actually has
-- once we have the closure we can apply it with the remaining arguments
    -- I define a variable that gets sequenced before the call which holds the closure
    -- so that it doesn't need to be computed many times
-- If a closure function is passed as an argument it needs a closure allocation -> (Var _ i) case
applyClosuresExpr :: CExpression a -> CStatement b -> ClosureFuns -> ClosureParams -> State Int (CStatement a, CExpression a)
applyClosuresExpr (Var t i) _ closureFuns _ =
    case Map.lookup i closureFuns of
        Just _ -> return (AllocClosure i, Val (ClosureV i))
        _ -> return (Skip, Var t i)
applyClosuresExpr (CallExpr tf tx f x) stmt closureFuns closureParams =
    let (f', args) = CDefs.collectArgs (CallExpr tf tx f x)
    in case f' of
        Var _ i ->
            case Map.lookup i closureFuns of
                Just innerFun -> do -- the called function returns a closure
                    let newType = fromMaybe CTVoidPtr (getFunType stmt (followClosureIFun innerFun closureFuns))
                    let mergedMap = Map.map length (getFunsWithParams stmt)
                    let numArgs = Map.findWithDefault 1 i mergedMap
                    let (currArgs, otherArgs) = splitAt numArgs args
                    (pre, currArgs') <- applyClosuresArgs currArgs stmt closureFuns closureParams
                    (pre', otherArgs') <- applyClosuresArgs otherArgs stmt closureFuns closureParams
                    let closVar = DefVar CTClosure i (rebuildCall tf f' currArgs')
                    (castStmt, castExpr) <- applyWithCast newType (Val (ClosureV i)) otherArgs'
                    return (unsafeCoerce (Seq pre (Seq pre' (Seq closVar castStmt))), castExpr)
                _ -> -- it does not return a closure
                    if i `elem` closureParams  -- check if it is a closure itself
                    then do
                        (pre, args') <- applyClosuresArgs args stmt closureFuns closureParams
                        (pre', stmt') <- applyWithCast CTVoidPtr f' args'
                        return $ unsafeCoerce (Seq pre' pre, stmt')
                    else
                        do
                        (pre, f'') <- applyClosuresExpr f stmt closureFuns closureParams
                        (pre', x') <- applyClosuresExpr x stmt closureFuns closureParams
                        return (unsafeCoerce $ Seq pre (unsafeCoerce pre'), CallExpr tf tx f'' x')
        _ -> do
            (pre, f'') <- applyClosuresExpr f stmt closureFuns closureParams
            (pre', x') <- applyClosuresExpr x stmt closureFuns closureParams
            return (unsafeCoerce $ Seq pre (unsafeCoerce pre'), CallExpr tf tx f'' x')
applyClosuresExpr (Ternary t c x y) stmt closureFuns closureParams = do
    (pre, c') <- applyClosuresExpr c stmt closureFuns closureParams
    (pre', x') <- applyClosuresExpr x stmt closureFuns closureParams
    (pre'', y') <- applyClosuresExpr y stmt closureFuns closureParams
    return (unsafeCoerce $ Seq pre (unsafeCoerce $ Seq pre' pre''), Ternary t c' x' y')
applyClosuresExpr (LIntOp op x y) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    (pre', y') <- applyClosuresExpr y stmt closureFuns closureParams
    return (Seq pre pre', LIntOp op x' y')
applyClosuresExpr (LCmpOp op x y) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    (pre', y') <- applyClosuresExpr y stmt closureFuns closureParams
    return (unsafeCoerce Seq pre pre', LCmpOp op x' y')
applyClosuresExpr (LBoolOp op x y) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    (pre', y') <- applyClosuresExpr y stmt closureFuns closureParams
    return (Seq pre pre', LBoolOp op x' y')
applyClosuresExpr (ConsList t x y) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    (pre', y') <- applyClosuresExpr y stmt closureFuns closureParams
    return (unsafeCoerce $ Seq pre (unsafeCoerce pre'), ConsList t x' y')
applyClosuresExpr (Not x) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    return (pre, Not x')
applyClosuresExpr (Abs x) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    return (pre, Abs x')
applyClosuresExpr (Box t x) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    return (unsafeCoerce pre, Box t x')
applyClosuresExpr (Unbox t x) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    return (unsafeCoerce pre, Unbox t x')
applyClosuresExpr (CastExpr t x) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    return (unsafeCoerce pre, CastExpr t x')
applyClosuresExpr (TailList t x) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    return (pre, TailList t x')
applyClosuresExpr (HeadList t x) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    return (unsafeCoerce pre, HeadList t x')
applyClosuresExpr (IsEmpty t x) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    return (unsafeCoerce pre, IsEmpty t x')
applyClosuresExpr (Fst t1 t2 x) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    return (unsafeCoerce pre, Fst t1 t2 x')
applyClosuresExpr (Snd t1 t2 x) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    return (unsafeCoerce pre, Snd t1 t2 x')
applyClosuresExpr (IndexList t x y) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    (pre', y') <- applyClosuresExpr y stmt closureFuns closureParams
    return (unsafeCoerce (Seq pre (unsafeCoerce pre')), IndexList t x' y')
applyClosuresExpr (Prod t x y) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    (pre', y') <- applyClosuresExpr y stmt closureFuns closureParams
    return (unsafeCoerce (Seq pre (unsafeCoerce pre')), Prod t x' y')
applyClosuresExpr (ApplyClosure t x y) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    (pre', y') <- applyClosuresExpr y stmt closureFuns closureParams
    return (unsafeCoerce (Seq pre (unsafeCoerce pre')), ApplyClosure t x' y')
applyClosuresExpr x _ _ _ = return (Skip, x)

applyClosuresArgs :: [CArg] -> CStatement a -> ClosureFuns -> ClosureParams -> State Int (CStatement a, [CArg])
applyClosuresArgs [] _ _ _ = return (Skip, [])
applyClosuresArgs [CArg t x] stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns  closureParams
    return (unsafeCoerce pre, [CArg t x'])
applyClosuresArgs (arg : rest) stmt closureFuns closureParams = do
    (pre, arg') <- applyClosuresArgs [arg] stmt closureFuns closureParams
    (pre', rest') <- applyClosuresArgs rest stmt closureFuns closureParams
    return (Seq pre pre', arg' ++ rest')

applyClosures :: CStatement a -> CStatement a -> ClosureFuns -> ClosureParams -> State Int (CStatement a)
applyClosures (DefFun tret ifun params body) stmt closureFuns closureParams = do
    body' <- applyClosures body stmt closureFuns closureParams
    return $ DefFun tret ifun params body'
applyClosures (Seq x y) stmt closureFuns closureParams = do
    x' <- applyClosures x stmt closureFuns closureParams
    y' <- applyClosures y stmt closureFuns closureParams
    return $ Seq x' y'
applyClosures (If c x y) stmt closureFuns closureParams = do
    (pre, c') <- applyClosuresExpr c stmt closureFuns closureParams
    x' <- applyClosures x stmt closureFuns closureParams
    y' <- applyClosures y stmt closureFuns closureParams
    return $ unsafeCoerce $ Seq pre (unsafeCoerce $ If c' x' y')
applyClosures (While c x) stmt closureFuns closureParams = do
    (pre, c') <- applyClosuresExpr c stmt closureFuns closureParams
    x' <- applyClosures x stmt closureFuns closureParams
    return $ unsafeCoerce $ Seq pre (unsafeCoerce $ While c' x')
applyClosures (BindExpr t c i x) stmt closureFuns closureParams = do
    (pre, c') <- applyClosuresExpr c stmt closureFuns closureParams
    x' <- applyClosures x stmt closureFuns closureParams
    return $ unsafeCoerce $ Seq pre (unsafeCoerce $ BindExpr t c' i x')
applyClosures (Return x) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    return $ Seq pre (Return x')
applyClosures (DefVar t i x) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    return $ unsafeCoerce $ Seq pre (DefVar t i x')
applyClosures (UpdateVar t i x) stmt closureFuns closureParams = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams
    return $ unsafeCoerce $ Seq pre (UpdateVar t i x')
applyClosures x _ _ _ = return x


-- keep looping make closure factories
-- after we made the first ones and applied closures some funs return closures now
-- so we need to handle them until nothing changes
applyClosuresPasses :: CStatement a -> ParentParams -> Map.Map Int Int -> Int -> CStatement a
applyClosuresPasses body parentParamsMap parents freshCounter =
    let (body', closureFuns) = makeClosureFactories body parentParamsMap parents Map.empty
    in  if Map.null closureFuns then body'
        else
            let body''' = evalState (applyClosures body' body' closureFuns Set.empty) freshCounter
            in applyClosuresPasses body''' parentParamsMap parents freshCounter


-- OPTIMISATIONS

-- Generate Structs + Pair Defs

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

generateEnvStructs :: Int -> Map.Map Int (Set.Set CParam) -> CStatement a
generateEnvStructs ifun liftenv =
    let envParams = maybe [] Set.toList (Map.lookup ifun liftenv)
    in DefEnvStruct ifun envParams

-- MAIN

removeFirstReturn :: CStatement a -> CStatement a
removeFirstReturn (Return _)           = Skip
removeFirstReturn (Seq x y)            = Seq x (removeFirstReturn y)
removeFirstReturn (BindExpr t x i y)   = BindExpr t x i (removeFirstReturn y)
removeFirstReturn (If c t e)           = If c (removeFirstReturn t) (removeFirstReturn e)
removeFirstReturn (While c x)          = While c (removeFirstReturn x)
removeFirstReturn x                    = x

splitTopLevel :: CStatement a -> (CStatement a, CStatement a)
splitTopLevel (Seq l@DefFun{} y) =
    let (funs, body) = splitTopLevel y
    in (Seq l funs, body)
splitTopLevel (Seq l@DefEnvStruct{} y) =
    let (funs, body) = splitTopLevel y
    in (Seq l funs, body)
splitTopLevel (Seq Skip y) = splitTopLevel y
splitTopLevel (Seq x y) =
    let (funs, body)   = splitTopLevel x
        (funs', body') = splitTopLevel y
    in (Seq funs funs', Seq body body')
splitTopLevel Skip = (Skip, Skip)
splitTopLevel l@DefFun{} = (l, Skip)
splitTopLevel x = (Skip, x)

translateALToC :: Typeable a => AL.Lang a -> (CStatement a, Int)
translateALToC progCode =
    let (nl, fresh') = runState (NL.translate progCode) 0
        (clBase, fresh'') = runState (CL.translate nl) fresh'
        clOpt = CL.optimizeBindings clBase
        c = translate clOpt
    in (c, fresh'')

runLiftAndMerge :: Bool -> CStatement a -> Int -> (CStatement a, ParentParams)
runLiftAndMerge canMerge body freshInt =
    let (body', (parentParamsMap, parentMap)) =
            if canMerge then
                let merged = mergeLambdas body body
                in runState (lambdaLift merged) (Map.empty, Map.empty)
            else runState (lambdaLift body) (Map.empty, Map.empty)
        body'' = addEnvParameter body' parentParamsMap
        body''' = applyClosuresPasses body'' parentParamsMap parentMap freshInt
        body'''' = addEnvAllocs body''' body''' parentParamsMap
        body''''' = addBoxing body''''
    in (body''''', parentParamsMap)

printCode :: Typeable a => CStatement a -> ParentParams -> String
printCode finalBody parentParams =
    let finalDefs = getDefs finalBody
        finalMergeMap = Map.map length (getFunsWithParams finalBody)
        globalInfo = getGlobalInfo finalBody emptyGlobalInfo

        envStructs = foldr (Seq . (`generateEnvStructs` parentParams)) Skip (Set.toList (usedEnvs globalInfo))
        
        imports =   "\n#include <stdbool.h>" ++
                    "\n#include <stdio.h>" ++
                    "\n#include <stdlib.h>" ++
                    "\n#include <stdint.h>" ++
                    "\n#include \"../listLib.c\"\n"
        (funPart, mainBody) = splitTopLevel finalBody
        retExpr = fromMaybe (error "no return") (findReturn mainBody)
        mainBodyWithoutRet = removeFirstReturn mainBody
        retImpl = showCExpression retExpr finalMergeMap
        mainBodyImpl = showCStmt 1 finalMergeMap mainBodyWithoutRet
        funImpl = showCStmt 0 finalMergeMap funPart

        in "\n// imports" ++ imports ++
            "\n// pair type defitions" ++ concatMap genPairDeclaration (Set.toList (pairTypes globalInfo)) ++
            "\n// function defitions" ++ showFunDefs finalDefs ++
            "\n\n// closure defitions" ++ showCStmt 0 Map.empty envStructs ++
            "\n// function implementations" ++ funImpl ++
            "\n// main\nint main(void) {" ++ mainBodyImpl ++
                    case show (typeRep mainBody) of
                        "Int" -> "\n  printInt("
                        "[Int]" -> "\n  printListInt("
                        _ -> error "cannot print"
            ++ retImpl ++ ");\n" ++ "  return 0;\n}\n"
