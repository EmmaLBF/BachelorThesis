{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Avoid lambda" #-}
-- {-# HLINT ignore "Replace case with fromMaybe" #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}

module C where

import CLang (indentStr)
import qualified AbsLang as AL
import qualified NamedLang as NL
import qualified CLang as CL

import Data.Dynamic
import Control.Monad.State
import Data.Typeable
import Debug.Trace
import System.IO
import Unsafe.Coerce
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

data CParam where
  CParam :: Int -> CType -> CParam
  CParamEnv  :: Int -> CParam -- void* env parameter

instance Eq CParam where
  CParam i _ == CParam j _ = i == j
  CParamEnv i == CParamEnv j = i == j
  CParam i _ == CParamEnv j = i == j
  CParamEnv i == CParam j _ = i == j

type CParams = [CParam]
type CParamMap = Map.Map Int CParam

data CType
    = CTInt
    | CTBool
    | CTVoid
    | CTNode
    | CTNodeInt
    | CTNodeBool
    | CTPair CType CType
    | CTClosure
    | CTPtr CType
    | CTFun CType CType
    | CTVoidPtr
    deriving (Show, Eq, Ord)

data CArg where
  CArg :: CType -> CExpression a -> CArg

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    UnitV :: CValue ()
    FunV  :: (CValue a -> CValue b) -> CValue (a -> b)
    PairV :: CValue a -> CValue b -> CValue (a, b)
    ListV :: [CValue a] -> CValue [a]
    ClosureV :: Int -> CValue a
    EnvV :: Int -> CValue a

data CExpression a where
    Val :: CValue a -> CExpression a
    Not :: CExpression Bool -> CExpression Bool
    Var :: CType -> Int -> CExpression a
    LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
    LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
    Ternary :: CType -> CExpression Bool -> CExpression a -> CExpression a -> CExpression a
    -- Tuples
    Prod :: CType -> CExpression a -> CExpression b -> CExpression (a, b)
    Fst :: CType -> CType -> CExpression (a, b) -> CExpression a -- holds type of res (a) and whole pair (a,b)
    Snd :: CType -> CType -> CExpression (a, b) -> CExpression b
    -- Lists
    EmptyList :: CType -> CExpression [a]
    ConsList :: CType -> CExpression a -> CExpression [a] -> CExpression [a]
    HeadList :: CType -> CExpression [a] -> CExpression a
    TailList :: CType -> CExpression [a] -> CExpression [a]
    IsEmpty :: CType -> CExpression [a] -> CExpression Bool
    IndexList :: CType -> CExpression [a] -> CExpression Int -> CExpression a
    -- Lambda
    ApplyClosure :: CType -> CExpression a -> CExpression b -> CExpression c  -- apply(f, arg), type of arg passed
    GetEnvField :: CType -> Int -> Int -> CExpression a  -- ((Env_vN*)env)->vM, with type for cast
    CallExpr :: CType -> CType -> CExpression (a -> b) -> CExpression a -> CExpression b
    -- Casting
    CastExpr :: CType -> CExpression a -> CExpression b
    Box :: CType -> CExpression a -> CExpression b
    Unbox :: CType -> CExpression a -> CExpression b

data CStatement a where
    Return :: CExpression a -> CStatement a
    BindExpr :: CType -> CExpression a -> Int -> CStatement b -> CStatement b
    Seq :: CStatement a -> CStatement a -> CStatement a
    If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a
    DefFun :: CType -> Int -> CParams -> CStatement b -> CStatement b
    DefVar :: CType -> Int -> CExpression a -> CStatement b
    UpdateVar :: CType -> Int -> CExpression a -> CStatement b
    While :: CExpression Bool -> CStatement a -> CStatement a
    Skip :: CStatement a
    DefClosureStruct :: Int -> CParams -> CStatement a  -- same, but fields are concrete types
    AllocClosure :: Int -> CStatement a -- closureId
    AllocEnv :: Int -> Int -> CParams -> CParams -> CStatement a -- envId parentId directParams parentParams

type LiftEnv = Map.Map Int CParams
type Lifted a = [CStatement a]

type ClosureReturnEnv = Map.Map Int Int

type Hoisted a = [CStatement a]
type FunTypes = Map.Map Int CType

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
addBoxingExpr (Ternary tp c t e) = Ternary tp (addBoxingExpr c) (addBoxingExpr t) (addBoxingExpr e)
addBoxingExpr (Not x) = Not (addBoxingExpr x)
addBoxingExpr (IsEmpty t x) = IsEmpty t (addBoxingExpr x)
addBoxingExpr (CastExpr t x) = CastExpr t (addBoxingExpr x)
addBoxingExpr (CallExpr tf tx f x) = CallExpr tf tx (addBoxingExpr f) (addBoxingExpr x)
addBoxingExpr (TailList t x) = TailList t (addBoxingExpr x)
addBoxingExpr (IndexList t i x) = IndexList t i (addBoxingExpr x)
addBoxingExpr (Prod t x y) = Prod t (addBoxingExpr x) (addBoxingExpr y)
addBoxingExpr x = x

-- LAMBDA LIFTING

--------- FREE VARS

paramsToMap :: CParams -> CParamMap
paramsToMap = Map.fromList . Prelude.map toEntry
  where
    toEntry p@(CParam i _) = (i, p)
    toEntry p@(CParamEnv i) = (i, p)

merge :: (CParamMap, CParamMap) -> (CParamMap, CParamMap) -> (CParamMap, CParamMap)
merge (xfree, xbound) (yfree, ybound) = (Map.union xfree yfree, Map.union xbound ybound)

-- free, bound
freeVarsExpr :: CExpression a -> (CParamMap, CParamMap)
freeVarsExpr (Not x) = freeVarsExpr x
freeVarsExpr (Fst _ _ x) = freeVarsExpr x
freeVarsExpr (Snd _ _ x) = freeVarsExpr x
freeVarsExpr (Box _ x) = freeVarsExpr x
freeVarsExpr (Unbox _ x) = freeVarsExpr x
freeVarsExpr (IsEmpty _ l) = freeVarsExpr l
freeVarsExpr (TailList _ l) = freeVarsExpr l
freeVarsExpr (HeadList _ l) = freeVarsExpr l
freeVarsExpr (IndexList _ l _) = freeVarsExpr l
freeVarsExpr (Val _) = (Map.empty, Map.empty)
freeVarsExpr (EmptyList _) = (Map.empty, Map.empty)
freeVarsExpr CastExpr {} =  (Map.empty, Map.empty)
freeVarsExpr ApplyClosure {} =  (Map.empty, Map.empty)
freeVarsExpr GetEnvField {} =  (Map.empty, Map.empty)
freeVarsExpr (Prod _ x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (LIntOp _ x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (LCmpOp _ x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (CallExpr _ _ f x) = merge (freeVarsExpr f) (freeVarsExpr x)
freeVarsExpr (ConsList _ l x) = merge (freeVarsExpr l) (freeVarsExpr x)
freeVarsExpr (Var t i) = (Map.singleton i (CParam i t), Map.empty)
freeVarsExpr (Ternary _ cond thn els) = merge (merge (freeVarsExpr cond) (freeVarsExpr thn)) (freeVarsExpr els)

freeVarsStmt :: CStatement a -> (CParamMap, CParamMap)
freeVarsStmt (BindExpr t x i y) =
    let (mfree, mbound) = merge (freeVarsExpr x) (freeVarsStmt y)
    in (mfree, Map.insert i (CParam i t) mbound)
freeVarsStmt (Seq x y) = merge (freeVarsStmt x) (freeVarsStmt y)
freeVarsStmt (If cond x y) = merge (freeVarsExpr cond) (merge (freeVarsStmt x) (freeVarsStmt y))
freeVarsStmt (While cond x) = merge (freeVarsExpr cond) (freeVarsStmt x)
freeVarsStmt (DefFun _ ifun params body) =
    let (bfree, bbound) = freeVarsStmt body
        boundKeys = paramsToMap params
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
freeVarsStmt Skip = (Map.empty, Map.empty)
freeVarsStmt _ = (Map.empty, Map.empty)

freeVars :: CStatement a -> CParamMap
freeVars s =
    let (free, bound) = freeVarsStmt s
    in Map.difference free bound

--------- HOISTING HELPERS

paramId :: CParam -> Int
paramId (CParam i _)  = i
paramId (CParamEnv i) = i

findReturn :: CStatement a -> Maybe (CExpression a)
findReturn (Return x) = Just x
findReturn (BindExpr _ _ _ y) = findReturn y
findReturn (Seq _ y) = findReturn y
findReturn (If _ x _) = findReturn x
findReturn (While _ x) = findReturn x
findReturn _ = Nothing

findDefFun :: Int -> CStatement a -> Maybe Int
findDefFun i (Seq (DefFun _ ifun1 _ _) _) | ifun1 == i = Just ifun1
findDefFun i (Seq _ y) = findDefFun i y
findDefFun i (BindExpr _ _ _ y) = findDefFun i y
findDefFun _ _ = Nothing

-- Extract the id of the inner function/closure that a DefFun body returns,
-- whether it came from a lambda (DefFun pattern) or an Apply (BindExpr pattern).
getInnerFunId :: CStatement a -> Maybe Int
getInnerFunId body =
    case findReturn body of
        Just (Var _ ret1) -> findDefFun ret1 body
        _ -> Nothing

outermostVar :: CExpression a -> Maybe Int
outermostVar (CallExpr _ _ f _) = outermostVar f
outermostVar (Var _ f) = Just f
outermostVar _ = Nothing

--------- HOISTING

rebuildCall :: CType -> CExpression a -> [CArg] -> CExpression a
rebuildCall tf = foldl (\acc (CArg ta a) -> CallExpr tf ta (unsafeCoerce acc) a)

collectArgs :: CExpression a -> (CExpression a, [CArg])
collectArgs (CallExpr _ tx f x) =
    let (f', as) = collectArgs (unsafeCoerce f)
    in (f', as ++ [CArg tx x])
collectArgs e = (e, [])

-- number of hops through map depends on num of args
collectArgsApply :: CExpression a -> (CExpression a, [CArg])
collectArgsApply (ApplyClosure tx f x) =
    let (f', as) = collectArgsApply (unsafeCoerce f)
    in (f', as ++ [CArg tx x])
collectArgsApply e = (e, [])

applyWithCast :: CType -> CExpression a -> [CArg] -> CExpression b
applyWithCast _ base [] = unsafeCoerce base
applyWithCast retType base [CArg t a] = CastExpr retType (ApplyClosure t base a)
applyWithCast retType base (CArg t a : rest) = applyWithCast retType (ApplyClosure t base a) rest

hoistClosureAllocs :: Int -> LiftEnv -> ClosureReturnEnv -> FunTypes -> CExpression a -> (Hoisted b, CExpression a)
hoistClosureAllocs ifun env closureRet funs expr@(CallExpr tf _ _ _) =
    let (func, args) = collectArgs expr
        (argAllocs, args') = unzip $ map (\(CArg t a) ->
            let (allocs, a') = hoistClosureAllocs ifun env closureRet funs a
            in (allocs, CArg t a')) args
        allArgAllocs = concat argAllocs
    in case func of
        myVar@(Var tvar ivar) ->
            let ownExtraPs    = Map.findWithDefault [] ivar env
                parentExtraPs = Map.findWithDefault [] ifun env
                retType =   let hops = length args'
                                follow 0 fId = Map.findWithDefault CTClosure fId funs
                                follow n fId = case Map.lookup fId closureRet of
                                                    Just fId' -> follow (n-1) fId'
                                                    Nothing   -> Map.findWithDefault CTClosure fId funs
                            in follow hops ivar
                closureVar    = Val (ClosureV ivar)
                directPs      = ownExtraPs \\ parentExtraPs
                parentPs      = ownExtraPs `intersect` parentExtraPs
                alloc         = [AllocEnv ivar ifun directPs parentPs, AllocClosure ivar]
            in case Map.lookup ivar closureRet of
                Just _ ->
                    let (factoryArgs, applyArgs') = splitAt (length directPs) args'
                    in  if null ownExtraPs
                        then -- no captures: first applyArg is plain C call, rest via ApplyClosure
                            let applied = case applyArgs' of
                                    [] -> rebuildCall tvar myVar factoryArgs
                                    (first : rest) ->
                                        let firstApplied = rebuildCall tvar myVar [first]
                                        in applyWithCast retType firstApplied rest
                            in (allArgAllocs, applied)
                        -- has captures: alloc closure, apply all args
                        else (allArgAllocs ++ alloc, applyWithCast retType closureVar args')
                Nothing ->
                    if null ownExtraPs
                        then (allArgAllocs, rebuildCall tvar myVar args')
                        else (allArgAllocs ++ alloc, applyWithCast retType closureVar args')
        _ -> (allArgAllocs, rebuildCall tf func args')
hoistClosureAllocs ifun env closureRet funs (Ternary tp c t e) =
    let (ca, c') = hoistClosureAllocs ifun env closureRet funs c
        (ta, t') = hoistClosureAllocs ifun env closureRet funs t
        (ea, e') = hoistClosureAllocs ifun env closureRet funs e
    in (ca ++ ta ++ ea, Ternary tp c' t' e')
hoistClosureAllocs ifun env closureRet funs (Not x) =
    let (a, x') = hoistClosureAllocs ifun env closureRet funs x
    in (a, Not x')
hoistClosureAllocs ifun env closureRet funs (IsEmpty t x) =
    let (a, x') = hoistClosureAllocs ifun env closureRet funs x
    in (a, IsEmpty t x')
hoistClosureAllocs ifun env closureRet funs (HeadList t x) =
    let (a, x') = hoistClosureAllocs ifun env closureRet funs x
    in (a, HeadList t x')
hoistClosureAllocs ifun env closureRet funs (TailList t x) =
    let (a, x') = hoistClosureAllocs ifun env closureRet funs x
    in (a, TailList t x')
hoistClosureAllocs ifun env closureRet funs (Fst tp tr x) =
    let (a, x') = hoistClosureAllocs ifun env closureRet funs x
    in (a, Fst tp tr x')
hoistClosureAllocs ifun env closureRet funs (Snd tp tr x) =
    let (a, x') = hoistClosureAllocs ifun env closureRet funs x
    in (a, Snd tp tr x')
hoistClosureAllocs ifun env closureRet funs (Prod t f g) =
    let (fa, f') = hoistClosureAllocs ifun env closureRet funs f
        (ga, g') = hoistClosureAllocs ifun env closureRet funs g
    in (fa ++ ga, Prod t f' g')
hoistClosureAllocs ifun env closureRet funs (CastExpr f g) =
    let (ga, g') = hoistClosureAllocs ifun env closureRet funs g
    in (ga, CastExpr f g')
hoistClosureAllocs ifun env closureRet funs (IndexList t f g) =
    let (fa, f') = hoistClosureAllocs ifun env closureRet funs f
        (ga, g') = hoistClosureAllocs ifun env closureRet funs g
    in (fa ++ ga, IndexList t f' g')
hoistClosureAllocs ifun env closureRet funs (ConsList t f g) =
    let (fa, f') = hoistClosureAllocs ifun env closureRet funs f
        (ga, g') = hoistClosureAllocs ifun env closureRet funs g
    in (fa ++ ga, ConsList t f' g')
hoistClosureAllocs ifun env closureRet funs (ApplyClosure tx f g) =
    let (fa, f') = hoistClosureAllocs ifun env closureRet funs f
        (ga, g') = hoistClosureAllocs ifun env closureRet funs g
    in (fa ++ ga, ApplyClosure tx f' g')
hoistClosureAllocs ifun env closureRet funs (LIntOp op f g) =
    let (fa, f') = hoistClosureAllocs ifun env closureRet funs f
        (ga, g') = hoistClosureAllocs ifun env closureRet funs g
    in (fa ++ ga, LIntOp op f' g')
hoistClosureAllocs ifun env closureRet funs (LCmpOp op f g) =
    let (fa, f') = hoistClosureAllocs ifun env closureRet funs f
        (ga, g') = hoistClosureAllocs ifun env closureRet funs g
    in (fa ++ ga, LCmpOp op f' g')
hoistClosureAllocs ifun env _ _ expr@(Var _ f) =
    let ownExtraPs    = Map.findWithDefault [] f env
        parentExtraPs = Map.findWithDefault [] ifun env
        directPs      = ownExtraPs \\ parentExtraPs
        parentPs      = ownExtraPs `intersect` parentExtraPs
        alloc         = [AllocEnv f ifun directPs parentPs, AllocClosure f]
    in if null ownExtraPs
       then ([], expr)             -- no captures, plain function pointer, leave as-is
       else (alloc, Val (ClosureV f))  -- has captures, emit alloc, replace with c
hoistClosureAllocs _ _ _ _ x = ([], x)

rewriteExpr :: Int -> LiftEnv -> ClosureReturnEnv -> FunTypes -> CExpression a -> CExpression a
rewriteExpr ifun env closureRet funs (CallExpr tf tx (f :: CExpression (arg -> a)) x) =
    let x' = rewriteExpr ifun env closureRet funs x
        oneHopRetType fId = case Map.lookup fId closureRet of
            Just fId' -> case Map.lookup fId' funs of
                Just t  -> t
                Nothing -> oneHopRetType fId'   -- follow the chain further until you find smth in funs, otherwise we get stuck in the lets
            Nothing   -> Map.findWithDefault CTClosure fId funs
    in case f of
        myVar@(Var tvar fId) ->
            case tvar of
                CTClosure -> applyWithCast (oneHopRetType fId) myVar [CArg tx x']
                _ ->
                    case (Map.lookup fId env, Map.member fId closureRet) of
                        (Nothing, True) ->
                            case Map.lookup fId funs of
                                Just _  -> CallExpr tf tx myVar x'  -- top-level fn, direct call
                                Nothing -> applyWithCast (oneHopRetType fId) myVar [CArg tx x']
                        _ -> CallExpr tf tx (rewriteExpr ifun env closureRet funs myVar) x'  -- local closure var, use apply
        _ ->
            let f' = rewriteExpr ifun env closureRet funs f
            in case outermostVar f of
                Just fId | Map.member fId closureRet ->
                    if Map.member fId env
                    then CallExpr tf tx f' x'
                    else applyWithCast (oneHopRetType fId) f' [CArg tx x']
                _ -> CallExpr tf tx f' x'
rewriteExpr ifun m closureRet funs  (Not x) = Not (rewriteExpr ifun m closureRet funs x)
rewriteExpr ifun m closureRet funs  (Fst tp tr x) = Fst tp tr (rewriteExpr ifun m closureRet funs x)
rewriteExpr ifun m closureRet funs  (Snd tp tr x) = Snd tp tr (rewriteExpr ifun m closureRet funs x)
rewriteExpr ifun m closureRet funs  (IsEmpty t x) = IsEmpty t (rewriteExpr ifun m closureRet funs x)
rewriteExpr ifun m closureRet funs  (HeadList t x) = HeadList t (rewriteExpr ifun m closureRet funs x)
rewriteExpr ifun m closureRet funs  (TailList t x) = TailList t (rewriteExpr ifun m closureRet funs x)
rewriteExpr ifun m closureRet funs  (IndexList t l i) = IndexList t (rewriteExpr ifun m closureRet funs l) i
rewriteExpr ifun m closureRet funs  (Prod t x y) = Prod t (rewriteExpr ifun m closureRet funs x) (rewriteExpr ifun m closureRet funs y)
rewriteExpr ifun m closureRet funs  (ConsList t l x) = ConsList t (rewriteExpr ifun m closureRet funs l) (rewriteExpr ifun m closureRet funs x)
rewriteExpr ifun m closureRet funs  (LIntOp op x y) = LIntOp op (rewriteExpr ifun m closureRet funs x) (rewriteExpr ifun m closureRet funs y)
rewriteExpr ifun m closureRet funs  (LCmpOp op x y) = LCmpOp op (rewriteExpr ifun m closureRet funs x) (rewriteExpr ifun m closureRet funs y)
rewriteExpr ifun m closureRet funs  (Ternary tp x y z) = Ternary tp (rewriteExpr ifun m closureRet funs x) (rewriteExpr ifun m closureRet funs y) (rewriteExpr ifun m closureRet funs z)
rewriteExpr ifun m _  _ (Var t i) =
    case Map.lookup ifun m of -- check if the var is part of the current params or in an env
        Just extraPs -> case findParam extraPs i of
            Just (CParam _ pt) -> GetEnvField pt ifun i
            _                  -> Var t i
        _ -> Var t i
        where
            findParam :: CParams -> Int -> Maybe CParam
            findParam [] _ = Nothing
            findParam [CParam ip tp] toFind
                | ip == toFind = Just (CParam ip tp)
                | otherwise = Nothing
            findParam (p:rest) toFind =
                let param = findParam [p] toFind
                in case param of
                    Nothing -> findParam rest toFind
                    _ -> param
rewriteExpr _ _ _ _ x = x

rewriteStmt :: Int -> LiftEnv -> ClosureReturnEnv -> FunTypes -> CStatement a -> CStatement a
rewriteStmt ifun m closureRet funs (Seq x y) = Seq (rewriteStmt ifun m closureRet funs x) (rewriteStmt ifun m closureRet funs y)
rewriteStmt ifun m closureRet funs (While cond x) = While (rewriteExpr ifun m closureRet funs cond) (rewriteStmt ifun m closureRet funs x)
rewriteStmt _ m closureRet funs (DefFun tret ifun1 params body) = DefFun tret ifun1 params (rewriteStmt ifun1 m closureRet funs body)
rewriteStmt ifun m closureRet funs (DefVar t i x) = DefVar t i (rewriteExpr ifun m closureRet funs x)
rewriteStmt ifun env closureRet funs (Return x) =
    let x' = rewriteExpr ifun env closureRet funs x
        (allocs, x'') = hoistClosureAllocs ifun env closureRet funs x'
    in foldr (Seq . unsafeCoerce) (Return x'') allocs
rewriteStmt ifun env closureRet funs (BindExpr t x i y) =
    let x' = rewriteExpr ifun env closureRet funs x
        (allocs, x'') = hoistClosureAllocs ifun env closureRet funs x'
        y' = rewriteStmt ifun env closureRet funs y
    in foldr (Seq . unsafeCoerce) (BindExpr t x'' i y') allocs
rewriteStmt ifun env closureRet funs (If cond t f) =
    let cond' = rewriteExpr ifun env closureRet funs cond
        (allocs, cond'') = hoistClosureAllocs ifun env closureRet funs cond'
    in foldr (Seq . unsafeCoerce) (If cond'' (rewriteStmt ifun env closureRet funs t)
        (rewriteStmt ifun env closureRet funs f)) allocs
rewriteStmt ifun env closureRet funs (UpdateVar t i x) =
    let x' = rewriteExpr ifun env closureRet funs x
        (allocs, x'') = hoistClosureAllocs ifun env closureRet funs x'
    in foldr (Seq . unsafeCoerce) (UpdateVar t i x'') allocs
rewriteStmt _ _ _ _ x = x

liftStmt :: Int -> LiftEnv -> ClosureReturnEnv -> FunTypes -> CStatement a -> (LiftEnv, ClosureReturnEnv, FunTypes, Lifted a, CStatement a)
liftStmt _ env closureRet funs (DefFun tret ifun params body) =
    let freeMapRaw = freeVars (DefFun tret ifun params body)
        freeMap = Map.withoutKeys freeMapRaw (Map.keysSet funs)
        extraPs = Map.elems freeMap
        newParams = case extraPs of
            [] -> params
            _  -> CParamEnv ifun : params
        env' = Map.insert ifun extraPs env
    in  case getInnerFunId body of
            Just ifun1 ->
                let closureRet' =
                        let innerExtraPs = Map.findWithDefault [] ifun1 env''
                        in if null innerExtraPs then closureRet
                            else Map.insert ifun ifun1 closureRet
                    funs' = case tret of
                        CTFun _ b -> Map.insert ifun b funs
                        _         -> Map.insert ifun tret funs
                    (env'', closureRet'', funs'', lifted, body') = liftStmt ifun env' closureRet' funs' body
                    (body'', closureRet''') =
                        let innerExtraPs = Map.findWithDefault [] ifun1 env''
                            paramIds = map paramId params
                            directPs = filter (\p -> paramId p `elem` paramIds) innerExtraPs
                            parentPs = innerExtraPs \\ directPs
                        in if null innerExtraPs
                            then (body', closureRet'')
                            else (Seq (AllocEnv ifun1 ifun directPs parentPs) (Seq (AllocClosure ifun1) (Return (Val (ClosureV ifun1))))
                                , Map.insert ifun ifun1 closureRet'')
                    thisDef =
                        let innerExtraPs = Map.findWithDefault [] ifun1 env''
                        in if null innerExtraPs
                            then DefFun tret ifun newParams body''
                            else DefFun CTClosure ifun newParams body''
                    funs''' = if null (Map.findWithDefault [] ifun1 env'')
                                then funs''
                                else Map.insert ifun (Map.findWithDefault CTClosure ifun1 funs'') funs''
                in (env'', closureRet''', funs''' , lifted ++ [thisDef], Skip)
            Nothing ->
                let funs' = Map.insert ifun tret funs
                    (env'', closureRet'', funs'', lifted, body') = liftStmt ifun env' closureRet funs' body
                    thisDef = DefFun tret ifun newParams body'
                in (env'', closureRet'', funs'', lifted ++ [thisDef], Skip)
liftStmt fun env closureRet funs (Seq x y) =
    let (env', closureRet', funs',  lx, x') = liftStmt fun env closureRet funs  x
        (env'', closureRet'', funs'',  ly, y') = liftStmt fun env' closureRet' funs' y
    in (env'', closureRet'', funs'', lx ++ ly, Seq x' y')
liftStmt fun env closureRet funs (If cond x y) =
    let (env', closureRet', funs',  lx, x') = liftStmt fun env closureRet funs  x
        (env'', closureRet'', funs'',  ly, y') = liftStmt fun env' closureRet' funs' y
    in (env'', closureRet'', funs'', lx ++ ly, If (rewriteExpr fun env closureRet'' funs'' cond) x' y')
liftStmt fun env closureRet funs (While cond x) =
    let (env', closureRet', funs', lx, x') = liftStmt fun env closureRet funs x
    in (env', closureRet', funs', lx, While (rewriteExpr fun env closureRet' funs' cond) x')
liftStmt fun env closureRet funs (BindExpr t x i y) =
    let -- first pass: determine closureRet1 before we know x''
        closureRet1 = case outermostVar x of
            Just f | Map.member f closureRet -> Map.insert i f closureRet
            Just f | not (null (Map.findWithDefault [] f env)) -> Map.insert i f closureRet
            _ -> closureRet
        (env', closureRet', funs', ly, y') = liftStmt fun env closureRet1 funs y
        x' = rewriteExpr fun env' closureRet' funs' x
        (allocs, x'') = hoistClosureAllocs fun env' closureRet' funs' x'
        -- after hoisting, x'' might be Val (ClosureV f) — update closureRet for t'
        t' = case x'' of
            Val (ClosureV _) -> CTClosure
            Var _ f | Map.member f closureRet ->
                case t of
                    CTFun arg _ -> CTFun arg CTClosure
                    _           -> t
            _ -> case outermostVar x'' of
                    Just f | not (null (Map.findWithDefault [] f env')) -> CTClosure
                    Just f | Map.member f closureRet' -> CTClosure
                    _ -> case x'' of
                            ApplyClosure {} -> CTClosure
                            CastExpr ct _   -> ct
                            _               -> t
    in (env', closureRet', funs', ly, foldr (Seq . unsafeCoerce) (BindExpr t' x'' i y') allocs)
liftStmt fun env closureRet funs s = (env, closureRet, funs, [], rewriteStmt fun env closureRet funs s)

lambdaLift :: CStatement a -> (CStatement a, ClosureReturnEnv, LiftEnv, FunTypes, [CStatement a])
lambdaLift stmt =
    let (env, closureRet, funs, lifted, stmt') = liftStmt (-1) Map.empty Map.empty Map.empty stmt
    in (Prelude.foldr Seq stmt' lifted, closureRet, env, funs, lifted)


-- OPTIMISATIONS

-- for a function (int), given the amount of params, check that every call site has at least that many applications
checkCallExpr :: Int -> Int -> CExpression a -> Bool
checkCallExpr fun params expr =
    let (f, args) = collectArgs expr
    in case f of
        Var _ i | i == fun -> length args >= params
        _ -> True

checkCallStmt :: Int -> Int -> CStatement a -> Bool
checkCallStmt fun params stmt = case stmt of
    Return e -> checkCallExpr fun params e
    Seq x y -> checkCallStmt fun params x && checkCallStmt fun params y
    If c t e -> checkCallExpr fun params c &&
                checkCallStmt fun params t &&
                checkCallStmt fun params e
    BindExpr _ e _ s -> checkCallExpr fun params e && checkCallStmt fun params s
    DefFun _ _ _ b -> checkCallStmt fun params b
    While c b -> checkCallExpr fun params c && checkCallStmt fun params b
    DefVar _ _ b -> checkCallExpr fun params b
    UpdateVar _ _ b -> checkCallExpr fun params b
    _  -> True

-- retrun merged and map of functions to their new number of params
-- the whole program unchanged, the current stmt, map of changed params
mergeLambdas :: CStatement b -> CStatement a -> Map.Map Int Int -> (CStatement a, Map.Map Int Int)
mergeLambdas prog (DefFun tret ifun params body) m =
    case body of
        (Seq (DefFun tret1 ifun1 params1 body1) (Return (Var _ i))) ->
            let newParams = params ++ params1
                canMerge = checkCallStmt ifun (length newParams) prog
            in if canMerge && ifun1 == i then
                let newDef = DefFun tret1 ifun newParams body1
                    newMap = Map.insert ifun (length newParams) m
                in mergeLambdas prog newDef newMap
                else let (body', m') = mergeLambdas prog body m
                    in (DefFun tret ifun params body', m')
        _ -> let (body', m') = mergeLambdas prog body m
            in (DefFun tret ifun params body', m')
mergeLambdas prog (Seq x y) m =
    let (x', m')  = mergeLambdas prog x m
        (y', m'') = mergeLambdas prog y m'
    in (Seq x' y', m'')
mergeLambdas prog (BindExpr t x i y) m =
    let (y', m') = mergeLambdas prog y m
    in (BindExpr t x i y', m')
mergeLambdas prog (If c x y) m =
    let (x', m')  = mergeLambdas prog x m
        (y', m'') = mergeLambdas prog y m'
    in (If c x' y', m'')
mergeLambdas prog (While x y) m =
    let (y', m') = mergeLambdas prog y m
    in (While x y', m')
mergeLambdas _ stmt m = (stmt, m)

-- MAKE PAIR DEFS
collectPairTypes :: CStatement a -> Set.Set (CType, CType) -> Set.Set (CType, CType)
collectPairTypes (DefFun _ _ _ body) s = collectPairTypes body s
collectPairTypes (Seq x y) s = collectPairTypes x (collectPairTypes y s)
collectPairTypes (If c x y) s = collectPairTypes x (collectPairTypes y (collectPairTypesExpr c s))
collectPairTypes (While c x) s = collectPairTypes x (collectPairTypesExpr c s)
collectPairTypes (Return x) s = collectPairTypesExpr x s
collectPairTypes (DefVar _ _ x) s = collectPairTypesExpr x s
collectPairTypes (UpdateVar _ _ x) s = collectPairTypesExpr x s
collectPairTypes (BindExpr _ c _ x) s = collectPairTypes x (collectPairTypesExpr c s)
collectPairTypes _ s = s

collectPairTypesExpr :: CExpression a -> Set.Set (CType, CType) -> Set.Set (CType, CType)
collectPairTypesExpr (Prod t x y) s = 
    case t of
        CTPtr (CTPair tx ty) -> Set.insert (tx, ty) (collectPairTypesExpr x (collectPairTypesExpr y s))
        CTPair tx ty -> Set.insert (tx, ty) (collectPairTypesExpr x (collectPairTypesExpr y s))
        _ -> collectPairTypesExpr x (collectPairTypesExpr y s)
collectPairTypesExpr (Not x) s = collectPairTypesExpr x s
collectPairTypesExpr (HeadList _ x) s = collectPairTypesExpr x s
collectPairTypesExpr (TailList _ x) s = collectPairTypesExpr x s
collectPairTypesExpr (IsEmpty _ x) s = collectPairTypesExpr x s
collectPairTypesExpr (IndexList _ _ x) s = collectPairTypesExpr x s
collectPairTypesExpr (Fst _ _ x) s = collectPairTypesExpr x s
collectPairTypesExpr (Snd _ _ x) s = collectPairTypesExpr x s
collectPairTypesExpr (Box _ x) s = collectPairTypesExpr x s
collectPairTypesExpr (Unbox _ x) s = collectPairTypesExpr x s
collectPairTypesExpr (CastExpr _ x) s = collectPairTypesExpr x s
collectPairTypesExpr (ConsList _ x y) s = collectPairTypesExpr x (collectPairTypesExpr y s)
collectPairTypesExpr (LIntOp _ x y) s = collectPairTypesExpr x (collectPairTypesExpr y s)
collectPairTypesExpr (LCmpOp _ x y) s = collectPairTypesExpr x (collectPairTypesExpr y s)
collectPairTypesExpr (CallExpr _ _ x y) s = collectPairTypesExpr x (collectPairTypesExpr y s)
collectPairTypesExpr (Ternary _ x y z) s = collectPairTypesExpr x (collectPairTypesExpr y (collectPairTypesExpr z s))
collectPairTypesExpr _ s = s

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
        


-- SHOW

-- convert haskell type to my CType
fromTypeRep :: TypeRep -> CType
fromTypeRep p =
    let args = typeRepArgs p
        con = show (typeRepTyCon p)
    in case (con, args) of
        ("Int", []) -> CTInt
        ("Bool", []) -> CTBool
        ("()", []) -> CTVoid
        ("[]", [a]) | show a == "Int" -> CTNodeInt
                    | show a == "Bool" -> CTNodeBool
        ("[]", [_]) -> CTNode
        ("(,)", [l,r]) -> CTPtr (CTPair (fromTypeRep l) (fromTypeRep r))
        ("->", [a, b]) -> CTFun (fromTypeRep a) (fromTypeRep b)
        _  -> CTVoidPtr

printPairType :: CType -> String
printPairType x = case x of
    CTInt -> "Int"
    CTBool -> "Bool"
    CTVoid -> "Void"
    CTNode -> "Node"
    CTNodeInt -> "NodeInt"
    CTNodeBool -> "NodeBool"
    CTPair _ _ -> "Pair"
    CTClosure -> "CLosure"
    CTPtr ct -> printPairType ct ++ "Ptr"
    CTFun ct ct' -> "Fun" ++ printPairType ct ++ printPairType ct'
    CTVoidPtr -> "VoidPtr"

-- print type for var decl, give string "v" + id
printDecl :: String -> CType -> String
printDecl name CTInt = "int " ++ name
printDecl name CTBool = "bool " ++ name
printDecl name CTVoid = "void* " ++ name
printDecl name CTNode = "Node* " ++ name
printDecl name CTNodeInt = "NodeInt* " ++ name
printDecl name CTNodeBool = "NodeBool* " ++ name
printDecl name (CTPair tl tr) = "Pair_" ++ printPairType tl ++ "_" ++ printPairType tr ++ " " ++ name
printDecl name CTClosure = "Closure* " ++ name
printDecl name CTVoidPtr = "void* " ++ name
printDecl name (CTPtr t) = printDecl ("*" ++ name) t
printDecl name (CTFun a b) = printFunPtr name a b

-- print type for function pointers, need to have recursive ()* with args
printFunPtr :: String -> CType -> CType -> String
printFunPtr name arg ret =
    case ret of
        CTFun a2 b2 -> printFunPtr ("(*" ++ name ++ ")(" ++ printType arg ++ ")") a2 b2
        _ -> printType ret ++ " (*" ++ name ++ ")(" ++ printType arg ++ ")"

printType :: CType -> String
printType CTInt = "int"
printType CTBool = "bool"
printType CTVoid = "void"
printType CTNode = "Node*"
printType CTNodeInt = "NodeInt*"
printType CTNodeBool = "NodeBool*"
printType (CTPair tl tr) = "Pair_" ++ printPairType tl ++ "_" ++ printPairType tr
printType CTClosure = "Closure*"
printType CTVoidPtr = "void*"
printType (CTPtr t) = printType t ++ "*"
printType (CTFun a b) = printType b ++ " (*)(" ++ printType a ++ ")"

showCParams :: CParams -> String
showCParams params =
    let hasEnv = any isEnv params
    in intercalate ", " (map (showParam hasEnv) params)
  where
    isEnv (CParamEnv _) = True
    isEnv _ = False
    showParam _ (CParamEnv i) = "void* env" ++ show i
    showParam True (CParam i _) = "void* v" ++ show i ++ "_raw" -- closure function, use void*
    showParam False (CParam i t) = printDecl ("v" ++ show i) t  -- plain function, keep type

showProxFunc :: String -> CParams -> CType -> String
showProxFunc name params (CTFun arg ret) =
    printType ret ++ " (*" ++ name ++ "(" ++ showCParams params ++ "))(" ++ printType arg ++ ")"
showProxFunc name params ct =
    printType ct ++ " " ++ name ++ "(" ++ showCParams params ++ ")"

box :: CType -> String -> String
box CTInt  e = "box_int(" ++ e ++ ")"
box CTBool e = "box_bool(" ++ e ++ ")"
box _      e = e

unbox :: CType -> String -> String
unbox CTInt  e = "*(int*)" ++ e
unbox CTBool e = "*(bool*)" ++ e
unbox t      e = "(" ++ printType t ++ ")" ++ e

boxForApply :: CType -> String -> String
boxForApply CTInt  e = "box_int(" ++ e ++ ")"
boxForApply CTBool e = "box_bool(" ++ e ++ ")"
boxForApply _      e = "(void*)(" ++ e ++ ")"

showCValue :: CValue a -> String
showCValue (IntV n)  = show n
showCValue (BoolV b) = show b
showCValue UnitV = "NULL"
showCValue (PairV x y) = "{ .fst = " ++ showCValue x ++ ", .snd = " ++ showCValue y ++ "}"
showCValue (FunV _) = "NULL"
showCValue (ListV l) =
  case l of
    [] -> ""
    (h:t) -> showCValue h ++ ", " ++ showCValue (ListV t)
showCValue (ClosureV i) = "c" ++ show i
showCValue (EnvV i) = "env" ++ show i

showCArg :: CArg -> Map.Map Int Int -> String
showCArg (CArg _ a) = showCExpression a

instance Show CArg where
    show (CArg t x) = showCArg (CArg t x) Map.empty

showListLibFunType :: CType -> String
showListLibFunType CTInt = "Int"
showListLibFunType CTBool = "Bool"
showListLibFunType _ = ""

showCExpression :: CExpression a -> Map.Map Int Int -> String
showCExpression (EmptyList _) _ = "NULL"
showCExpression (Val v) _ = showCValue v
showCExpression (Var _ i) _ = "v" ++ show i
showCExpression (Not x) m = "!(" ++ showCExpression x m ++ ")"
showCExpression (LIntOp op x y) m = "(" ++ showCExpression x m ++ " " ++ CL.showBinOp op ++ " " ++ showCExpression y m ++ ")"
showCExpression (LCmpOp op x y) m = "(" ++ showCExpression x m ++ " " ++ CL.showCmpOp op ++ " " ++ showCExpression y m ++ ")"
showCExpression (Box t x) m = box t (showCExpression x m)
showCExpression (Unbox t x) m = unbox t ("(" ++ showCExpression x m ++ ")")
showCExpression (Prod t l r) m = 
    case t of
        CTPair tl tr -> "(" ++ "Pair_" ++ printPairType tl ++ "_" ++ printPairType tr ++ "){ .fst = " ++ showCExpression l m ++ ", .snd = " ++ showCExpression r m ++ " }"
        CTPtr (CTPair tl tr) -> "makePair_" ++ printPairType tl ++ "_" ++ printPairType tr ++ "(" ++ showCExpression l m ++ ", " ++ showCExpression r m ++ ")"
        _ -> error "not valid type " ++ printType t
showCExpression (Fst tp _ p) m = "(" ++ showCExpression p m ++
    case tp of
        CTPair _ _ -> ").fst"
        _ -> ")->fst"
showCExpression (Snd tp _ p) m = "(" ++ showCExpression p m ++
    case tp of
        CTPair _ _ -> ").snd"
        _ -> ")->snd"
showCExpression (IsEmpty _ l) m = "((" ++ showCExpression l m ++ ") == NULL)"
showCExpression (HeadList _ l) m = "(" ++ showCExpression l m ++ ")->head"
showCExpression (TailList _ l) m = "(" ++ showCExpression l m ++ ")->tail"
showCExpression (ConsList t x l) m = "cons" ++ showListLibFunType t ++ "(" ++ showCExpression x m ++ ", " ++ showCExpression l m ++ ")"
showCExpression (IndexList _ l i) m = showCExpression l m ++ "[" ++ showCExpression i m ++ "]"
showCExpression (Ternary _ cond thn els) m = "((" ++ showCExpression cond m ++ ") ? (" ++ showCExpression thn m ++ ") : (" ++ showCExpression els m ++ "))"
showCExpression (GetEnvField _ structId fieldId) _ = "((Env_v" ++ show structId ++ "*)env" ++ show structId ++ ")->v" ++ show fieldId
showCExpression (CastExpr t x) m = case t of
    CTInt -> "(int)(intptr_t)" ++ showCExpression x m
    CTBool -> "(bool)(intptr_t)" ++ showCExpression x m
    _ -> "(" ++ printType t ++ ")" ++ showCExpression x m
showCExpression (ApplyClosure targ f arg) m =
    let (func, args) = collectArgsApply (ApplyClosure targ f arg)
        applyCall expr argList =
            "((Closure*)" ++ expr ++ ")->fn(" ++
            intercalate ", " (("((Closure*)" ++ expr ++ ")->env") : map (`showCArg` m) argList) ++ ")"
        n = case func of
            Var _ i -> Map.findWithDefault 1 i m
            Val (ClosureV i) -> Map.findWithDefault 1 i m
            _ -> 1
        (merged, rest) = splitAt n args
        baseCall = applyCall (showCExpression func m) merged
    in foldl (\acc arg' -> applyCall acc [arg']) baseCall rest
-- merges together nested calls if I merged together the params earlier
showCExpression (CallExpr tf tx f arg) m =
    let (func, args) = collectArgs (CallExpr tf tx f arg)
        formatArgs [] = []
        formatArgs (CArg _ (Val (EnvV j)) : rest) = ("env" ++ show j) : map (\(CArg t' a) ->
                boxForApply t' (showCExpression a m)) rest
        formatArgs args' = map (`showCArg` m) args'
    in case func of
        Var _ i -> case Map.lookup i m of
            Just n ->
                let (merged, rest) = Prelude.splitAt n args
                    baseCall = showCExpression func m ++ "(" ++ intercalate ", " (formatArgs merged) ++ ")"
                in if Prelude.null rest
                   then baseCall
                   else baseCall ++ "(" ++ intercalate ", " (formatArgs rest) ++ ")"
            Nothing -> foldl (\acc a -> acc ++ "(" ++ head (formatArgs [a]) ++ ")") (showCExpression func m) args
        _ -> foldl (\acc a -> acc ++ "(" ++ head (formatArgs [a]) ++ ")") (showCExpression func m) args

showCStmt :: Int -> Map.Map Int Int -> ClosureReturnEnv -> Map.Map Int String -> CStatement a -> String
showCStmt indent m _ _ (UpdateVar _ i x) = "\n" ++ indentStr indent ++ "v" ++ show i ++ " = " ++ showCExpression x m ++ ";"
showCStmt indent m closures funs (If cond t f) =
    "\n" ++ indentStr indent ++ "if (" ++ showCExpression cond m ++ ") {"
    ++  showCStmt (indent + 1) m closures funs t
    ++ "\n" ++ indentStr indent  ++ "} else {"
    ++ showCStmt (indent + 1) m closures funs f
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent m closures funs (While cond body) =
    "\n" ++ indentStr indent ++ "while " ++ showCExpression cond m ++ " {"
    ++ showCStmt (indent + 1) m closures funs body
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent m closures funs (BindExpr ct x i y) =
    "\n" ++ indentStr indent ++ printDecl ("v" ++ show i) ct
    ++ " = " ++ showCExpression x m ++ ";"
    ++ showCStmt indent m closures funs y
showCStmt indent m closures funs (Seq x y) = showCStmt indent m closures funs x ++ showCStmt indent m closures funs y
showCStmt indent m closures funs (DefFun ct ifun params body) =
    let hasEnv = any (\case CParamEnv _ -> True; _ -> False) params
        unboxings = if hasEnv
            then concatMap (\case
                CParam ip t ->
                    "\n" ++ indentStr (indent+1) ++
                    printDecl ("v" ++ show ip) t ++
                    " = " ++ unbox t ("v" ++ show ip ++ "_raw") ++ ";"
                _ -> "") params
            else ""
    in "\n" ++ indentStr indent ++ showProxFunc ("v" ++ show ifun) params ct ++ " {"
    ++ unboxings
    ++ showCStmt (indent + 1) m closures funs body
    ++ "\n" ++ indentStr indent ++ "}\n"
showCStmt indent m _ _ (DefVar ct i x) =
    "\n" ++ indentStr indent ++
        case x of
            (Val (EnvV _)) -> printDecl ("env" ++ show i) ct
            _ -> printDecl ("v" ++ show i) ct
    ++ " = " ++ showCExpression x m ++ ";"
showCStmt indent m _ _ (Return x) =  "\n" ++ indentStr indent ++ "return " ++ showCExpression x m ++ ";"
showCStmt indent _ _ _ (DefClosureStruct ifun p) =
    "\n" ++ indentStr indent ++ "typedef struct {\n"
    ++ concatMap (\case CParam ip tp -> "    " ++ printDecl ("v" ++ show ip) tp ++ ";\n"; _ -> "") p
    ++ "} Env_v" ++ show ifun ++ ";\n"
showCStmt indent _ _ _ (AllocClosure funId) =
    "\n" ++ indentStr indent ++ "Closure* c" ++ show funId ++ " = malloc(sizeof(Closure));"
    ++ "\n" ++ indentStr indent ++ "c" ++ show funId ++ "->env = env" ++ show funId ++ ";"
    ++ "\n" ++ indentStr indent ++ "c" ++ show funId
    ++ "->fn = (void* (*)(void*, void*))v" ++ show funId ++ ";"
showCStmt indent _ _ _ (AllocEnv envId parentId directParams parentParams) =
    "\n" ++ indentStr indent ++ "Env_v" ++ show envId ++ "* env" ++ show envId
        ++ " = malloc(sizeof(Env_v" ++ show envId ++ "));"
    ++ showDirect directParams
    ++ showParent parentParams
  where
    showDirect [] = ""
    showDirect (CParam ip _ : rest) =
        "\n" ++ indentStr indent ++ "env" ++ show envId ++ "->v" ++ show ip
        ++ " = " ++ "v" ++ show ip ++ ";"
        ++ showDirect rest
    showDirect (_ : rest) = showDirect rest
    showParent [] = ""
    showParent (CParam ip _ : rest) =
        "\n" ++ indentStr indent ++ "env" ++ show envId ++ "->v" ++ show ip
            ++ " = ((Env_v" ++ show parentId ++ "*)env" ++ show parentId ++ ")->v" ++ show ip ++ ";"
        ++ showParent rest
    showParent (_ : rest) = showParent rest
showCStmt _ _ _ _ Skip = ""

showFunDefs :: [CStatement a] -> String
showFunDefs [] = ""
showFunDefs [DefFun tret ifun params _] = "\n" ++ showProxFunc ("v" ++ show ifun) params tret ++ ";"
showFunDefs (i:is) = showFunDefs[i] ++ showFunDefs is

-------- DEBUG PRINTS

showFreeVars :: [CStatement a] -> String
showFreeVars [] = ""
showFreeVars [DefFun tret ifun params body] =
    let (bfree, bbound) = freeVarsStmt (DefFun tret ifun params body)
        i = freeVars (DefFun tret ifun params body)
    in "\nFunction " ++ show ifun
       ++ " | bfree=" ++ show (Map.keys bfree)
       ++ " | bbound=" ++ show (Map.keys bbound)
       ++ " | result=" ++ show (Map.keys i)
showFreeVars (i:is) = showFreeVars[i] ++ showFreeVars is

showLiftEnv :: [(Int, CParams)] -> String
showLiftEnv [] = ""
showLiftEnv [(i, p)] = "\nfun" ++ show i ++ " | " ++ "(" ++ showCParams p ++ ")"
showLiftEnv (i:is) = showLiftEnv [i] ++ showLiftEnv is

showParamMap :: [(Int, CParam)] -> String
showParamMap [] = ""
showParamMap [(i, p)] = "v" ++ show i ++ " (" ++ showCParams [p] ++ "), "
showParamMap (i:is) = showParamMap [i] ++ showParamMap is

showFunTypes :: [(Int, CType)] -> String
showFunTypes [] = ""
showFunTypes [(i,t)] = "v" ++ show i ++ " = " ++ printType t ++ "\n"
showFunTypes (i:is) = showFunTypes [i] ++ showFunTypes is

showCArgs :: [CArg] -> String
showCArgs [] = ""
showCArgs (CArg _ x : rest) = "CArg " ++ showCExpression x Map.empty ++ "\n" ++  showCArgs rest

showListStmt :: [CStatement a] -> String
showListStmt = concatMap (showCStmt 0 Map.empty Map.empty Map.empty)

-- MAIN

generateClosureStructs :: [CStatement a] -> LiftEnv -> CStatement a
generateClosureStructs [] _ = Skip
generateClosureStructs (DefFun _ ifun params _ : rest) liftenv =
    let envParams = case Map.lookup ifun liftenv of
                        Just ps -> ps
                        Nothing -> []
        allParams = params ++ envParams
    in Seq (DefClosureStruct ifun allParams) (generateClosureStructs rest liftenv)
generateClosureStructs _ _ = error "not valid def fun"

findFirstReturn :: CStatement a -> CExpression a
findFirstReturn (Return x)        = x
findFirstReturn (Seq _ y)         = findFirstReturn y
findFirstReturn (BindExpr _ _ _ y) = findFirstReturn y
findFirstReturn (If _ t _)        = findFirstReturn t  -- both branches should match
findFirstReturn (While _ x)       = findFirstReturn x
findFirstReturn _                 = error "no return found"

removeFirstReturn :: CStatement a -> CStatement a
removeFirstReturn (Return _)           = Skip
removeFirstReturn (Seq x y)            = Seq x (removeFirstReturn y)
removeFirstReturn (BindExpr t x i y)   = BindExpr t x i (removeFirstReturn y)
removeFirstReturn (If c t e)           = If c (removeFirstReturn t) (removeFirstReturn e)
removeFirstReturn (While c x)          = While c (removeFirstReturn x)
removeFirstReturn x                    = x

splitTopLevel :: CStatement a -> (CStatement a, CStatement a)
splitTopLevel (Seq l@DefFun {} y) =
    let (funs, body) = splitTopLevel y
    in (Seq l funs, body)
splitTopLevel (Seq l@DefClosureStruct {} y) =
    let (funs, body) = splitTopLevel y
    in (Seq l funs, body)
splitTopLevel (Seq Skip y) = splitTopLevel y
splitTopLevel (Seq x y) =
    let (funs, body) = splitTopLevel y
    in (funs, Seq x body)
splitTopLevel Skip = (Skip, Skip)
splitTopLevel x = (Skip, x)

getStrFunTypes :: [CStatement a] -> Map.Map Int String -> Map.Map Int String
getStrFunTypes [] m = m
getStrFunTypes ((DefFun tret ifun _ _) : rest) m =
    let m' = Map.insert ifun (printType tret) m
    in getStrFunTypes rest m'
getStrFunTypes (_ : rest) m = getStrFunTypes rest m


{-
gcc ./outputs/mergeSortCall_output.c -o ./outputs/mergeSortCall_output
./outputs/mergeSortCall_output
-}

run :: Typeable a => String -> AL.Lang a -> Bool -> IO ()
run progName progCode canMerge = do
    let libName = "\n#include \"" ++ "../"  ++ "listLib.c\"\n"
    let progPath =
            if canMerge then "mergedLams/" ++ progName
            else "baselines/" ++ progName
    -- let libName = "\n#include \"listLib.c\"\n"
    -- let progPath = progName

    let (nl, c') = NL.translate 0 progCode
        (clBase, _) = runState (CL.translate nl) c'
        (clOpt, newBinds) = CL.optimizeBindings clBase Map.empty
        clOptRepl = CL.replaceVarBindingStmt clOpt newBinds
        c = translate clOptRepl

    putStrLn "--- Translating to CLang ---"
    putStrLn $ CL.showCStmt 0 clBase

    let ((cbody', closureEnv, liftenv, _, defs), mergedMap) =
            if canMerge then
                let (merged, mergedLams) = mergeLambdas c c Map.empty
                in (lambdaLift merged, mergedLams)
            else (lambdaLift c, Map.empty)

    let cbody = addBoxing cbody'
    let strFunTypes = getStrFunTypes defs Map.empty

    putStrLn "\n--- Printing C ---"
    let imports =   "\n#include <stdbool.h>" ++
                    "\n#include <stdio.h>" ++
                    "\n#include <stdlib.h>" ++
                    "\n#include <stdint.h>" ++
                    libName

    let closureStructs = generateClosureStructs defs liftenv
    let funDefs = showFunDefs defs
    let (funPart, mainBody) = splitTopLevel cbody
    let retExpr = findFirstReturn mainBody
    let mainBodyWithoutRet = removeFirstReturn mainBody

    let retImpl = showCExpression retExpr mergedMap
    let mainBodyImpl = showCStmt 1 mergedMap closureEnv strFunTypes mainBodyWithoutRet
    let funImpl = showCStmt 0 mergedMap closureEnv strFunTypes funPart
    let pairTypes = collectPairTypes cbody Set.empty

    let content =
            "\n// imports" ++ imports ++
            "\n// pair type defitions" ++ concatMap genPairDeclaration (Set.toList pairTypes) ++
            "\n// function defitions" ++ funDefs ++
            "\n\n// closure defitions" ++ showCStmt 0 Map.empty Map.empty strFunTypes closureStructs ++
            "\n// function implementations" ++ funImpl ++
            "\n// main\nint main(void) {" ++ mainBodyImpl ++
                    case show (typeRep mainBody) of
                        "Int" -> "\n  printInt("
                        "[Int]" -> "\n  printListInt("
                        _ -> error "cannot print"
            ++ retImpl ++ ");\n" ++ "  return 0;\n}\n"

    -- writing to file
    let fileName = "outputs/" ++ progPath ++ ".c"
    handle <- openFile fileName WriteMode
    hPutStrLn handle content
    hClose handle
    putStrLn $ "Successfully wrote to " ++ fileName

main :: IO ()
main = do
    let progsInt = [("gcdLangCall", AL.gcdLangCall), ("fibCall", AL.fibCall), ("sumListCall", AL.sumListCall), ("lenListCall", AL.lenListCall)]
    let progsList = [("mapListCall", AL.mapListCall), ("mergeSortCall", AL.mergeSortCall)]

    -- let canMerge = True
    mapM_ (\(name, prog) -> run name prog False) progsInt
    mapM_ (\(name, prog) -> run name prog False) progsList

    mapM_ (\(name, prog) -> run name prog True) progsInt
    mapM_ (\(name, prog) -> run name prog True) progsList
