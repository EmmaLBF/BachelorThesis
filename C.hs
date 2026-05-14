{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# LANGUAGE LambdaCase #-}


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
  CParam :: Typeable a => Int -> Proxy a -> CParam
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
    | CTPair
    | CTClosure 
    | CTPtr CType
    | CTFun CType CType
    | CTVoidPtr
    deriving (Show, Eq)

fromTypeRep :: TypeRep -> CType
fromTypeRep p =
    let args = typeRepArgs p
        con  = show (typeRepTyCon p)
    in case (con, args) of
        ("Int",  [])     -> CTInt
        ("Bool", [])     -> CTBool
        ("()",   [])     -> CTVoid
        ("[]",   [_])    -> CTNode
        ("(,)",  [_,_])  -> CTPair
        ("->",   [a, b]) -> CTFun (fromTypeRep a) (fromTypeRep b)
        _                -> CTVoidPtr   -- unknown, erase

printDecl :: String -> CType -> String
printDecl name CTInt         = "int " ++ name
printDecl name CTBool        = "bool " ++ name
printDecl name CTVoid        = "void* " ++ name
printDecl name CTNode        = "Node* " ++ name
printDecl name CTPair        = "Pair* " ++ name
printDecl name CTClosure     = "Closure* " ++ name
printDecl name CTVoidPtr     = "void* " ++ name
printDecl name (CTPtr t)     = printDecl ("*" ++ name) t
printDecl name (CTFun a b)   = printFunPtr name a b

printFunPtr :: String -> CType -> CType -> String
printFunPtr name arg ret =
    case ret of
        CTFun a2 b2 -> printFunPtr ("(*" ++ name ++ ")(" ++ printType arg ++ ")") a2 b2
        _           -> printType ret ++ " (*" ++ name ++ ")(" ++ printType arg ++ ")"

printType :: CType -> String
printType CTInt     = "int"
printType CTBool    = "bool"
printType CTVoid    = "void"
printType CTNode    = "Node*"
printType CTPair    = "Pair*"
printType CTClosure = "Closure*"
printType CTVoidPtr = "void*"
printType (CTPtr t) = printType t ++ "*"
printType (CTFun a b) = printType b ++ " (*)(" ++ printType a ++ ")"

data CArg where
  CArg :: Typeable a => CExpression a -> CArg

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    UnitV :: CValue ()
    FunV  :: (CValue a -> CValue b) -> CValue (a -> b)
    PairV :: CValue a -> CValue b -> CValue (a, b)
    ListV :: [CValue a] -> CValue [a]
    ClosureV :: Int -> CValue a

data CExpression a where
    Val :: CValue a -> CExpression a
    Not :: CExpression Bool -> CExpression Bool
    Var :: Typeable a => CType -> Int -> CExpression a
    LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
    LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
    Ternary :: Typeable a => CExpression Bool -> CExpression a -> CExpression a -> CExpression a
    -- Tuples
    Prod :: (Typeable a, Typeable b) => CExpression a -> CExpression b -> CExpression (a, b)
    Fst  :: (Typeable a, Typeable b) => CExpression (a, b) -> CExpression a
    Snd  :: (Typeable a, Typeable b) => CExpression (a, b) -> CExpression b
    -- Lists
    EmptyList    :: Typeable a => CExpression [a]
    ConsList   :: Typeable a => CExpression a -> CExpression [a] -> CExpression [a]
    HeadList   :: Typeable a => CExpression [a] -> CExpression a
    TailList   :: Typeable a => CExpression [a] -> CExpression [a]
    IsEmpty  :: Typeable a => CExpression [a] -> CExpression Bool
    IndexList  :: Typeable a => CExpression [a] -> CExpression Int -> CExpression a
    -- Lambda
    ApplyClosure :: Typeable b => CExpression a -> CExpression b -> CExpression c  -- apply(f, arg)
    GetEnvField :: Typeable a => CType -> Int -> Int -> CExpression a  -- ((Env_vN*)env)->vM, with type for cast
    CallExpr :: (Typeable a, Typeable b) => CExpression (a -> b) -> CExpression a -> CExpression b
    -- Casting
    CastExpr :: CType -> CExpression a -> CExpression b

data CStatement a where
    Return :: CExpression a -> CStatement a
    BindExpr :: Typeable a => CType -> CExpression a -> Int -> CStatement b -> CStatement b
    Seq :: CStatement a -> CStatement a -> CStatement a
    If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a
    DefFun :: (Typeable b) => CType -> Int -> CParams -> CStatement b -> CStatement b
    DefVar :: Typeable a => CType -> Int -> CExpression a -> CStatement b
    UpdateVar :: Typeable a => Int -> CExpression a -> CStatement b
    While :: CExpression Bool -> CStatement a -> CStatement a
    Skip :: CStatement a
    DefClosureStruct :: Int -> CParams -> CStatement a  -- same, but fields are concrete types
    AllocClosure :: Int -> Int -> Int -> CParams -> CParams -> CStatement a -- structId  implId  directParams  parentEnvParams

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
translateExpr CL.EmptyList = EmptyList
translateExpr (CL.Not e) = Not (translateExpr e)
translateExpr (CL.Fst p) = Fst (translateExpr p)
translateExpr (CL.Snd p) = Snd (translateExpr p)
translateExpr (CL.Val v) = Val (translateValue v)
translateExpr (CL.IsEmpty l) = IsEmpty (translateExpr l)
translateExpr (CL.HeadList l) = HeadList (translateExpr l)
translateExpr (CL.TailList l) = TailList (translateExpr l)
translateExpr (CL.Prod a b) = Prod (translateExpr a) (translateExpr b)
translateExpr (CL.Var i) = Var (fromTypeRep (typeRep (Proxy :: Proxy a))) i
translateExpr (CL.ConsList h t) = ConsList (translateExpr h) (translateExpr t)
translateExpr (CL.CallExpr f x) = CallExpr (translateExpr f) (translateExpr x)
translateExpr (CL.IndexList l i) = IndexList (translateExpr l) (translateExpr i)
translateExpr (CL.LIntOp op e1 e2) = LIntOp op (translateExpr e1) (translateExpr e2)
translateExpr (CL.LCmpOp op e1 e2) = LCmpOp op (translateExpr e1) (translateExpr e2)
translateExpr (CL.Ternary c t f) = Ternary (translateExpr c) (translateExpr t) (translateExpr f)

translate :: CL.CStatement a -> CStatement a
translate CL.Skip = Skip
translate (CL.Return x) = Return (translateExpr x)
translate (CL.DefVar i (x :: CL.CExpression a)) = 
    DefVar (fromTypeRep (typeRep (Proxy :: Proxy a))) i (translateExpr x)
translate (CL.UpdateVar i x) = UpdateVar i (translateExpr x)
translate (CL.While cond x) = While (translateExpr cond) (translate x)
translate (CL.Seq x y) = Seq (translate x) (translate y)
translate (CL.BindExpr (x :: CL.CExpression a) i s) = 
    BindExpr (fromTypeRep (typeRep (Proxy :: Proxy a))) (translateExpr x) i (translate s)
translate (CL.If cond x y) = If (translateExpr cond) (translate x) (translate y)
translate (CL.DefFun tret ifun (ip, tp) body) = 
    DefFun (fromTypeRep (typeRep tret)) ifun [CParam ip tp] (translate body)

-- LAMBDA LIFTING

--------- FREE VARS

paramsToMap :: CParams -> CParamMap
paramsToMap = Map.fromList . Prelude.map toEntry
  where
    toEntry p@(CParam i _)        = (i, p)
    toEntry p@(CParamEnv i) = (i, p)

merge :: (CParamMap, CParamMap) -> (CParamMap, CParamMap) -> (CParamMap, CParamMap)
merge (xfree, xbound) (yfree, ybound) = (Map.union xfree yfree, Map.union xbound ybound)

-- free, bound
freeVarsExpr :: forall a. CExpression a -> (CParamMap, CParamMap)
freeVarsExpr (Not x) = freeVarsExpr x
freeVarsExpr (Fst x) = freeVarsExpr x
freeVarsExpr (Snd x) = freeVarsExpr x
freeVarsExpr (IsEmpty l) = freeVarsExpr l
freeVarsExpr (TailList l) = freeVarsExpr l
freeVarsExpr (HeadList l) = freeVarsExpr l
freeVarsExpr (IndexList l _) = freeVarsExpr l
freeVarsExpr (Val _) = (Map.empty, Map.empty)
freeVarsExpr EmptyList = (Map.empty, Map.empty)
freeVarsExpr CastExpr {} =  (Map.empty, Map.empty)
freeVarsExpr ApplyClosure {} =  (Map.empty, Map.empty)
freeVarsExpr GetEnvField {} =  (Map.empty, Map.empty)
freeVarsExpr (Prod x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (LIntOp _ x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (LCmpOp _ x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (CallExpr f x) = merge (freeVarsExpr f) (freeVarsExpr x)
freeVarsExpr (ConsList l x) = merge (freeVarsExpr l) (freeVarsExpr x)
freeVarsExpr (Var _ i) = (Map.singleton i (CParam i (Proxy :: Proxy a)), Map.empty)
freeVarsExpr (Ternary cond thn els) = merge (merge (freeVarsExpr cond) (freeVarsExpr thn)) (freeVarsExpr els)

freeVarsStmt :: Typeable a => CStatement a -> (CParamMap, CParamMap)
freeVarsStmt (BindExpr _ (x :: CExpression a) i y) =
    let (mfree, mbound) = merge (freeVarsExpr x) (freeVarsStmt y)
    in (mfree, Map.insert i (CParam i (Proxy :: Proxy a)) mbound)
freeVarsStmt (Seq x y) = merge (freeVarsStmt x) (freeVarsStmt y)
freeVarsStmt (If cond x y) = merge (freeVarsExpr cond) (merge (freeVarsStmt x) (freeVarsStmt y))
freeVarsStmt (While cond x) = merge (freeVarsExpr cond) (freeVarsStmt x)
freeVarsStmt (DefFun _ ifun params body) =
    let (bfree, bbound) = freeVarsStmt body
        boundKeys = paramsToMap params
        locallyBound = Map.insert ifun undefined boundKeys
        actualFree = Map.difference bfree locallyBound
    in (actualFree, Map.insert ifun undefined (Map.union bbound boundKeys))
freeVarsStmt (UpdateVar i (x :: CExpression a)) =
    let (xfree, xbound) = freeVarsExpr x
    in (Map.union (Map.singleton i (CParam i (Proxy :: Proxy a))) xfree, xbound)
freeVarsStmt (DefVar _ i (x :: CExpression a)) =
    let (xfree, xbound) = freeVarsExpr x
    in (xfree, Map.insert i (CParam i (Proxy :: Proxy a)) xbound)
freeVarsStmt (Return x) = freeVarsExpr x
freeVarsStmt Skip = (Map.empty, Map.empty)
freeVarsStmt _ = (Map.empty, Map.empty)

freeVars :: Typeable a => CStatement a -> CParamMap
freeVars s =
    let (free, bound) = freeVarsStmt s
    in Map.difference free bound

--------- HOISTING

step :: CExpression x -> CArg -> CExpression y
step acc (CArg a) =
  unsafeCoerce $ CallExpr
    (unsafeCoerce acc :: CExpression (Int -> Int))
    (unsafeCoerce a   :: CExpression Int)

rebuildCall :: CExpression a -> [CArg] -> CExpression a
rebuildCall = foldl step

collectArgs :: CExpression a -> (CExpression a, [CArg])
collectArgs (CallExpr f x) =
    let (f', as) = collectArgs (unsafeCoerce f)
    in (f', as ++ [CArg x])
collectArgs e = (e, [])

-- number of hops through map depends on num of args
collectArgsApply :: CExpression a -> (CExpression a, [CArg])
collectArgsApply (ApplyClosure f x) =
    let (f', as) = collectArgsApply (unsafeCoerce f)
    in (f', as ++ [CArg x])
collectArgsApply e = (e, [])

paramId :: CParam -> Int
paramId (CParam i _)  = i
paramId (CParamEnv i) = i

applyWithCast :: CType -> CExpression a -> [CArg] -> CExpression b
applyWithCast _ base [] = unsafeCoerce base
applyWithCast retType base [CArg a] =
    unsafeCoerce $ CastExpr retType (ApplyClosure (unsafeCoerce base) a)
applyWithCast retType base (CArg a : rest) =
    applyWithCast retType (ApplyClosure base a) rest

hoistClosureAllocs :: Int -> LiftEnv -> ClosureReturnEnv -> FunTypes -> CExpression a -> (Hoisted b, CExpression a)
hoistClosureAllocs ifun env closureRet funs (expr@(CallExpr _ _) :: CExpression a) =
    let (func, args) = collectArgs expr
        (argAllocs, args') = unzip $ map (\(CArg a) ->
            let (allocs, a') = hoistClosureAllocs ifun env closureRet funs a
            in (allocs, CArg a')) args
        allArgAllocs = concat argAllocs
    in case func of
        myVar@(Var _ f) ->
            let ownExtraPs    = Map.findWithDefault [] f env
                parentExtraPs = Map.findWithDefault [] ifun env
                retType =   let hops = length args'
                                follow 0 fId = Map.findWithDefault CTClosure fId funs
                                follow n fId = case Map.lookup fId closureRet of
                                                    Just fId' -> follow (n-1) fId'
                                                    Nothing   -> Map.findWithDefault CTClosure fId funs
                            in follow hops f
                closureVar    = unsafeCoerce (Val (ClosureV f) :: CExpression Int)
                directPs      = ownExtraPs \\ parentExtraPs
                parentPs      = ownExtraPs `intersect` parentExtraPs
                alloc         = AllocClosure f f ifun directPs parentPs
            in case Map.lookup f closureRet of
                Just _ ->
                    let (factoryArgs, applyArgs') = splitAt (length directPs) args'
                    in if null ownExtraPs
                        then -- no captures: first applyArg is plain C call, rest via ApplyClosure
                            let applied = case applyArgs' of
                                    [] -> unsafeCoerce $ rebuildCall myVar factoryArgs
                                    (first : rest) ->
                                        let firstApplied = rebuildCall myVar [first]
                                        in unsafeCoerce $ applyWithCast retType firstApplied rest
                            in (allArgAllocs, unsafeCoerce applied)
                        -- has captures: alloc closure, apply all args
                        else (allArgAllocs ++ [alloc], unsafeCoerce $ applyWithCast retType closureVar args')
                Nothing ->
                    if null ownExtraPs
                        then (allArgAllocs, unsafeCoerce $ rebuildCall myVar args')
                        else (allArgAllocs ++ [alloc], unsafeCoerce $ applyWithCast retType closureVar args')
        _ ->
            let rebuilt = rebuildCall (unsafeCoerce func) args'
            in (allArgAllocs, unsafeCoerce rebuilt)
hoistClosureAllocs ifun env closureRet funs (Ternary c t e) =
    let (ca, c') = hoistClosureAllocs ifun env closureRet funs c
        (ta, t') = hoistClosureAllocs ifun env closureRet funs t
        (ea, e') = hoistClosureAllocs ifun env closureRet funs e
    in (ca ++ ta ++ ea, Ternary c' t' e')
hoistClosureAllocs ifun env closureRet funs (Not x) =
    let (a, x') = hoistClosureAllocs ifun env closureRet funs x
    in (a, Not x')
hoistClosureAllocs ifun env closureRet funs (IsEmpty x) =
    let (a, x') = hoistClosureAllocs ifun env closureRet funs x
    in (a, IsEmpty x')
hoistClosureAllocs ifun env closureRet funs (HeadList x) =
    let (a, x') = hoistClosureAllocs ifun env closureRet funs x
    in (a, HeadList x')
hoistClosureAllocs ifun env closureRet funs (TailList x) =
    let (a, x') = hoistClosureAllocs ifun env closureRet funs x
    in (a, TailList x')
hoistClosureAllocs ifun env closureRet funs (Fst x) =
    let (a, x') = hoistClosureAllocs ifun env closureRet funs x
    in (a, Fst x')
hoistClosureAllocs ifun env closureRet funs (Snd x) =
    let (a, x') = hoistClosureAllocs ifun env closureRet funs x
    in (a, Snd x')
hoistClosureAllocs ifun env closureRet funs (Prod f g) =
    let (fa, f') = hoistClosureAllocs ifun env closureRet funs f
        (ga, g') = hoistClosureAllocs ifun env closureRet funs g
    in (fa ++ ga, Prod f' g')
hoistClosureAllocs ifun env closureRet funs (CastExpr f g) =
    let (ga, g') = hoistClosureAllocs ifun env closureRet funs g
    in (ga, CastExpr f g')
hoistClosureAllocs ifun env closureRet funs (IndexList f g) =
    let (fa, f') = hoistClosureAllocs ifun env closureRet funs f
        (ga, g') = hoistClosureAllocs ifun env closureRet funs g
    in (fa ++ ga, IndexList f' g')
hoistClosureAllocs ifun env closureRet funs (ConsList f g) =
    let (fa, f') = hoistClosureAllocs ifun env closureRet funs f
        (ga, g') = hoistClosureAllocs ifun env closureRet funs g
    in (fa ++ ga, ConsList f' g')
hoistClosureAllocs ifun env closureRet funs (ApplyClosure f g) =
    let (fa, f') = hoistClosureAllocs ifun env closureRet funs f
        (ga, g') = hoistClosureAllocs ifun env closureRet funs g
    in (fa ++ ga, ApplyClosure f' g')
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
        closureVar    = unsafeCoerce (Val (ClosureV f) :: CExpression Int)
        directPs      = ownExtraPs \\ parentExtraPs
        parentPs      = ownExtraPs `intersect` parentExtraPs
        alloc         = AllocClosure f f ifun directPs parentPs
    in if null ownExtraPs
       then ([], expr)             -- no captures, plain function pointer, leave as-is
       else ([alloc], unsafeCoerce closureVar)  -- has captures, emit alloc, replace with c
hoistClosureAllocs _ _ _ _ x = ([], x)

unCArg :: CArg -> CExpression a
unCArg (CArg (tmp :: CExpression a)) = unsafeCoerce tmp

rewriteExpr :: Int -> LiftEnv -> ClosureReturnEnv -> FunTypes -> CExpression a -> CExpression a
rewriteExpr ifun env closureRet funs (CallExpr (f :: CExpression (arg -> a)) x) =
    let x' = rewriteExpr ifun env closureRet funs x
        oneHopRetType fId = case Map.lookup fId closureRet of
            Just fId' -> case Map.lookup fId' funs of
                Just t  -> t
                Nothing -> oneHopRetType fId'   -- follow the chain further until you find smth in funs, otherwise we get stuck in the lets
            Nothing   -> Map.findWithDefault CTClosure fId funs
    in case f of
        myVar@(Var tf fId) ->
            case tf of
                CTClosure -> unsafeCoerce $ applyWithCast (oneHopRetType fId) myVar [CArg x']
                _ ->
                    case (Map.lookup fId env, Map.member fId closureRet) of
                        (Nothing, True) ->
                            case Map.lookup fId funs of
                                Just _  -> unsafeCoerce $ CallExpr myVar x'  -- top-level fn, direct call
                                Nothing -> unsafeCoerce $ applyWithCast (oneHopRetType fId) myVar [CArg x']
                        _ -> unsafeCoerce $ CallExpr (rewriteExpr ifun env closureRet funs myVar) x'  -- local closure var, use apply
        _ ->
            let f' = rewriteExpr ifun env closureRet funs f
            in case outermostVar f of
                Just fId | Map.member fId closureRet ->
                    if Map.member fId env
                    then CallExpr f' x'
                    else unsafeCoerce $ applyWithCast (oneHopRetType fId) f' [CArg x']
                _ -> CallExpr f' x'
rewriteExpr ifun m closureRet funs  (Not x) = Not (rewriteExpr ifun m closureRet funs x)
rewriteExpr ifun m closureRet funs  (Fst x) = Fst (rewriteExpr ifun m closureRet funs x)
rewriteExpr ifun m closureRet funs  (Snd x) = Snd (rewriteExpr ifun m closureRet funs x)
rewriteExpr ifun m closureRet funs  (IsEmpty x) = IsEmpty (rewriteExpr ifun m closureRet funs x)
rewriteExpr ifun m closureRet funs  (HeadList x) = HeadList (rewriteExpr ifun m closureRet funs x)
rewriteExpr ifun m closureRet funs  (TailList x) = TailList (rewriteExpr ifun m closureRet funs x)
rewriteExpr ifun m closureRet funs  (IndexList l i) = IndexList (rewriteExpr ifun m closureRet funs l) i
rewriteExpr ifun m closureRet funs  (Prod x y) = Prod (rewriteExpr ifun m closureRet funs x) (rewriteExpr ifun m closureRet funs y)
rewriteExpr ifun m closureRet funs  (ConsList l x) = ConsList (rewriteExpr ifun m closureRet funs l) (rewriteExpr ifun m closureRet funs x)
rewriteExpr ifun m closureRet funs  (LIntOp op x y) = LIntOp op (rewriteExpr ifun m closureRet funs x) (rewriteExpr ifun m closureRet funs y)
rewriteExpr ifun m closureRet funs  (LCmpOp op x y) = LCmpOp op (rewriteExpr ifun m closureRet funs x) (rewriteExpr ifun m closureRet funs y)
rewriteExpr ifun m closureRet funs  (Ternary x y z) = Ternary (rewriteExpr ifun m closureRet funs x) (rewriteExpr ifun m closureRet funs y) (rewriteExpr ifun m closureRet funs z)
rewriteExpr ifun m _  _ ((Var t i) :: CExpression a) =
    case Map.lookup ifun m of -- check if the var is part of the current params or in an env
        Just extraPs | findParam extraPs i ->
            GetEnvField t ifun i
        _ -> Var t i
        where
            findParam :: CParams -> Int -> Bool
            findParam [] _ = False
            findParam [CParam ip _] toFind = ip == toFind
            findParam (p:rest) toFind = findParam [p] toFind || findParam rest toFind
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
rewriteStmt ifun env closureRet funs (UpdateVar i x) =
    let x' = rewriteExpr ifun env closureRet funs x
        (allocs, x'') = hoistClosureAllocs ifun env closureRet funs x'
    in foldr (Seq . unsafeCoerce) (UpdateVar i x'') allocs
rewriteStmt _ _ _ _ x = x

-- Extract the id of the inner function/closure that a DefFun body returns,
-- whether it came from a lambda (DefFun pattern) or an Apply (BindExpr pattern).
getInnerFunId :: CStatement a -> Maybe Int
getInnerFunId body =
    case findReturn body of
        Just (Var _ ret1) -> findDefFun ret1 body
        _                 -> Nothing
  where
    findReturn :: CStatement a -> Maybe (CExpression a)
    findReturn (Return x)           = Just x
    findReturn (Seq _ y)            = findReturn y
    findReturn (BindExpr _ _ _ y)   = findReturn y
    findReturn _                    = Nothing

    findDefFun :: Int -> CStatement a -> Maybe Int
    findDefFun i (Seq (DefFun _ ifun1 _ _) _) | ifun1 == i = Just ifun1
    findDefFun i (Seq _ y)                                  = findDefFun i y
    findDefFun i (BindExpr _ _ _ y)                         = findDefFun i y
    findDefFun _ _                                          = Nothing

outermostVar :: CExpression a -> Maybe Int
outermostVar (CallExpr f _) = outermostVar (unsafeCoerce f)
outermostVar (Var _ f) = Just f
outermostVar _ = Nothing

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
                            else (Seq (AllocClosure ifun1 ifun1 ifun directPs parentPs)
                                    (Return (Val (ClosureV ifun1))), Map.insert ifun ifun1 closureRet'')
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
                -- closure-returning factory with no captures: fn ptr, but return type is Closure*
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
    UpdateVar _ b -> checkCallExpr fun params b
    _  -> True

-- retrun merged and map of functions to their new number of params
-- the whole program unchanged, the current stmt, map of changed params
mergeLambdas :: CStatement b -> CStatement a -> Map.Map Int Int -> (CStatement a, Map.Map Int Int)
mergeLambdas prog (DefFun tret ifun params body) m =
    case body of
        (Seq (DefFun tret1 ifun1 params1 body1) (Return (Var _ i))) ->
            let newParams = params ++ params1
                canMerge = checkCallStmt ifun (length newParams) prog
            in if canMerge && (ifun1 == i) then
                let newDef = unsafeCoerce $ DefFun tret1 ifun newParams (unsafeCoerce body1 :: CStatement Int)
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



-- SHOW

showStructDefVars :: CParams -> String
showStructDefVars [] = ""
showStructDefVars (CParam ip tp : rest) =
    "    " ++ printDecl ("v" ++ show ip) (fromTypeRep (typeRep tp)) ++ ";\n"
    ++ showStructDefVars rest
showStructDefVars (_ : rest) = showStructDefVars rest

showCParams :: CParams -> String
showCParams params =
    let hasEnv = any isEnv params
    in intercalate ", " (map (showParam hasEnv) params)
  where
    isEnv (CParamEnv _) = True
    isEnv _ = False
    showParam _ (CParamEnv _) = "void* env"
    showParam True (CParam i _) = "void* v" ++ show i ++ "_raw" -- closure function, use void*
    showParam False (CParam i t) = printDecl ("v" ++ show i) (fromTypeRep (typeRep t))  -- plain function, keep type

showCCast :: CType -> String -> String
showCCast CTInt  e = "(int)(intptr_t)" ++ e
showCCast CTBool e = "(bool)(intptr_t)" ++ e
showCCast t      e = "(" ++ printType t ++ ")" ++ e

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
showCValue (PairV x y) = "(" ++ showCValue x ++ ", " ++ showCValue y ++ ")"
showCValue (FunV _) = "funv"
showCValue (ListV l) =
  case l of
    [] -> ""
    (h:t) -> showCValue h ++ ", " ++ showCValue (ListV t)
showCValue (ClosureV _) = "c"

showCExpression :: CExpression a -> Map.Map Int Int -> String
showCExpression (Var _ i) _ = "v" ++ show i
showCExpression (Not x) m = "!(" ++ showCExpression x m ++ ")"
showCExpression (LIntOp op x y) m = "(" ++ showCExpression x m ++ " " ++ CL.showBinOp op ++ " " ++ showCExpression y m ++ ")"
showCExpression (LCmpOp op x y) m = "(" ++ showCExpression x m ++ " " ++ CL.showCmpOp op ++ " " ++ showCExpression y m ++ ")"
showCExpression (Val v) _ = showCValue v
showCExpression EmptyList _ = "NULL"
showCExpression (Prod (l :: CExpression a) (r :: CExpression b)) m =
    let l' = box  (fromTypeRep (typeRep (Proxy :: Proxy a))) (showCExpression l m)
        r' = box (fromTypeRep (typeRep (Proxy :: Proxy b))) (showCExpression r m)
    in "mk_pair(" ++ l' ++ ", " ++ r' ++ ")"
showCExpression (Fst (p :: CExpression (a, b))) m =
    unbox (fromTypeRep (typeRep (Proxy :: Proxy a))) ("fst(" ++ showCExpression p m ++ ")")
showCExpression (Snd (p :: CExpression (a, b))) m =
    unbox (fromTypeRep (typeRep (Proxy :: Proxy b))) ("snd(" ++ showCExpression p m ++ ")")
showCExpression (IsEmpty l) m = "isEmpty(" ++ showCExpression l m ++ ")"
showCExpression (HeadList (l :: CExpression [a])) m =
    unbox (fromTypeRep (typeRep (Proxy :: Proxy a))) ("(head(" ++ showCExpression l m ++ "))")
showCExpression (TailList l) m = "tail(" ++ showCExpression l m ++ ")"
showCExpression (ConsList (x :: CExpression a) l) m =
    let x' = box (fromTypeRep (typeRep (Proxy :: Proxy a))) (showCExpression x m)
    in "cons(" ++ x' ++ ", " ++ showCExpression l m ++ ")"
showCExpression (IndexList l i) m = showCExpression l m ++ "[" ++ showCExpression i m ++ "]"
showCExpression (Ternary cond thn els) m = "(" ++ showCExpression cond m ++ ") ? (" ++ showCExpression thn m ++ ") : (" ++ showCExpression els m ++ ")"
showCExpression (GetEnvField _ structId fieldId) _ =
    "((Env_v" ++ show structId ++ "*)env)->v" ++ show fieldId
showCExpression (CastExpr t x) m = showCCast t (showCExpression x m)
showCExpression (ApplyClosure f (arg :: CExpression b)) m =
    let (func, args) = collectArgsApply (ApplyClosure f arg)
        formatArg :: CArg -> String
        formatArg (CArg (a :: CExpression c)) =
            let argStr = showCExpression a m
            in boxForApply (fromTypeRep (typeRep (Proxy :: Proxy c))) argStr
    in case func of
        Var _ i -> case Map.lookup i m of
            Just n ->
                let (merged, rest) = splitAt n args
                    clos = "((Closure*)" ++ showCExpression func m  ++ ")"
                    fnCast = clos ++ "->fn"
                    allArgs = intercalate ", " ((clos ++ "->env") : map formatArg merged)
                    baseCall = fnCast ++ "(" ++ allArgs ++ ")"
                in if null rest
                then baseCall
                else    let clos2 = "((Closure*)" ++ baseCall ++ ")"
                            fn2 = clos2 ++ "->fn"
                        in fn2 ++ "(" ++ clos2 ++ "->env, " ++ formatArg (head rest) ++ ")"
            Nothing -> "apply((Closure*)" ++ showCExpression f m ++ ", " ++ formatArg (CArg arg) ++ ")"
        Val (ClosureV i) -> case Map.lookup i m of
            Just n ->
                let (merged, rest) = splitAt n args
                    clos = "((Closure*)" ++ showCExpression func m  ++ ")"
                    fnCast = clos ++ "->fn"
                    allArgs = intercalate ", " ((clos ++ "->env") : map formatArg merged)
                    baseCall = fnCast ++ "(" ++ allArgs ++ ")"
                in if null rest
                then baseCall
                else    let clos2 = "((Closure*)" ++ baseCall ++ ")"
                            fn2 = clos2 ++ "->fn"
                        in fn2 ++ "(" ++ clos2 ++ "->env, " ++ formatArg (head rest) ++ ")"
            Nothing -> "apply((Closure*)" ++ showCExpression f m ++ ", " ++ formatArg (CArg arg) ++ ")"
        _ -> "apply((Closure*)" ++ showCExpression f m ++ ", " ++ formatArg (CArg arg) ++ ")"

-- merges together nested calls if I merged together the params earlier
showCExpression (CallExpr f arg) m =
    let (func, args) = collectArgs (CallExpr f arg)
    in case func of
        Var _ i -> case Map.lookup i m of
            Just n ->
                -- trace ("SHOW MAP var" ++ show i ++ " = "  ++ show m) $
                let (merged, rest) = Prelude.splitAt n args
                    baseCall = "v" ++ show i ++ "(" ++ intercalate ", " (Prelude.map (\(CArg a) -> showCExpression a m) merged) ++ ")"
                in if Prelude.null rest
                   then baseCall
                   else baseCall ++ "(" ++ intercalate ", " (Prelude.map (\(CArg a) -> showCExpression a m) rest) ++ ")"
            Nothing ->
                foldl (\acc (CArg a) -> acc ++ "(" ++ showCExpression a m ++ ")")
                      ("v" ++ show i) args
        _ ->
            foldl (\acc (CArg a) -> acc ++ "(" ++ showCExpression a m ++ ")")
                  (showCExpression func m) args

showCStmt :: Int -> Map.Map Int Int -> ClosureReturnEnv -> Map.Map Int String -> CStatement a -> String
showCStmt indent m _ _ (UpdateVar i x) = "\n" ++ indentStr indent ++ "v" ++ show i ++ " = " ++ showCExpression x m ++ ";"
showCStmt indent m closures funs (If cond t f) =
    "\n" ++ indentStr indent ++ "if " ++ showCExpression cond m ++ " {"
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
                    let ct' = fromTypeRep (typeRep t)
                    in "\n" ++ indentStr (indent+1) ++
                       printDecl ("v" ++ show ip) ct' ++
                       " = " ++ unbox ct' ("v" ++ show ip ++ "_raw") ++ ";"
                _ -> "") params
            else ""
    in "\n" ++ indentStr indent ++ showProxFunc ("v" ++ show ifun) params ct ++ " {"
    ++ unboxings
    ++ showCStmt (indent + 1) m closures funs body
    ++ "\n" ++ indentStr indent ++ "}\n"
showCStmt indent m _ _ (DefVar ct i x) =
    "\n" ++ indentStr indent ++ printDecl ("v" ++ show i) ct
    ++ " = " ++ showCExpression x m ++ ";"
showCStmt indent m _ _ (Return x) =  "\n" ++ indentStr indent ++ "return " ++ showCExpression x m ++ ";"
showCStmt indent _ _ _ (DefClosureStruct ifun p) =
    "\n" ++ indentStr indent ++ "typedef struct {\n"
    ++ showStructDefVars p
    ++ "} Env_v" ++ show ifun ++ ";\n"
showCStmt indent _ _ _ (AllocClosure structId implId parentId directParams parentParams) =
    "\n" ++ indentStr indent ++ "Env_v" ++ show structId ++ "* env" ++ show structId
        ++ " = malloc(sizeof(Env_v" ++ show structId ++ "));"
    ++ showDirect directParams
    ++ showParent parentParams
    ++ "\n" ++ indentStr indent ++ "Closure* c = malloc(sizeof(Closure));"
    ++ "\n" ++ indentStr indent ++ "c->env = env" ++ show structId ++ ";"
    ++ "\n" ++ indentStr indent ++ "c"
    ++ "->fn = (void* (*)(void*, void*))v" ++ show implId ++ ";"
  where
    showDirect [] = ""
    showDirect (CParam ip _ : rest) =
        "\n" ++ indentStr indent ++ "env" ++ show structId ++ "->v" ++ show ip
        ++ " = " ++ "v" ++ show ip ++ ";"
        ++ showDirect rest
    showDirect (_ : rest) = showDirect rest
    showParent [] = ""
    showParent (CParam ip _ : rest) =
        "\n" ++ indentStr indent ++ "env" ++ show structId ++ "->v" ++ show ip
            ++ " = ((Env_v" ++ show parentId ++ "*)env)->v" ++ show ip ++ ";"
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
showCArgs (CArg x : rest) = "CArg " ++ showCExpression x Map.empty ++ "\n" ++  showCArgs rest

showListStmt :: [CStatement a] -> String
showListStmt = concatMap (showCStmt 0 Map.empty Map.empty Map.empty)

-- MAIN

generateClosureStructs :: [(Int, CParams)] -> CStatement a
generateClosureStructs [] = Skip
generateClosureStructs [(_, [])] = Skip
generateClosureStructs [(ifun, p)] = DefClosureStruct ifun p
generateClosureStructs (i:is) = Seq (generateClosureStructs [i]) (generateClosureStructs is)

findFirstReturn :: CStatement a -> CExpression a
findFirstReturn (Return x) = x
findFirstReturn (Seq x y) =
    case x of
        (Return i) -> i
        DefFun {} -> findFirstReturn y
        _ -> findFirstReturn y
findFirstReturn (BindExpr _ _ _ y) = findFirstReturn y
findFirstReturn _ = error "no return"

removeFirstReturn :: CStatement a -> CStatement a
removeFirstReturn (Return _) = Skip
removeFirstReturn (Seq (Return _) y) = Seq Skip y
removeFirstReturn (Seq x@DefFun {} y) = Seq x (removeFirstReturn y)
removeFirstReturn (Seq x y) = Seq x (removeFirstReturn y)
removeFirstReturn (BindExpr t x i y) = BindExpr t x i (removeFirstReturn y)
removeFirstReturn x = x

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
gcc ./outputs/fibCall_output.c -o ./outputs/fibCall_output
./outputs/fibCall_output

gcc ./outputs/gcdLangCall_output.c -o ./outputs/gcdLangCall_output
./outputs/gcdLangCall_output

gcc ./outputs/sumListCall_output.c -o ./outputs/sumListCall_output
./outputs/sumListCall_output

gcc ./outputs/lenListCall_output.c -o ./outputs/lenListCall_output
./outputs/lenListCall_output

gcc ./outputs/mapListCall_output.c -o ./outputs/mapListCall_output
./outputs/mapListCall_output

gcc ./outputs/mergeSortCall_output.c -o ./outputs/mergeSortCall_output
./outputs/mergeSortCall_output

fibCall
gcdLangCall
sumListCall
lenListCall
mapListCall
mergeSortCall
-}

main :: IO ()
main = do
    -- let libName = "\n#include \"" ++ "../"  ++ "listLib.c\"\n"
    let libName = "\n#include \"listLib.c\"\n"
    -- let progName = "merged/" ++ "sumListCall"
    let progName ="mergeSortCall_output"
    let (nl, c') = NL.translate 0 AL.mergeSortCall
        (clBase, _) = runState (CL.translate nl) c'
        (clOpt, newBinds) = CL.optimizeBindings clBase Map.empty
        clOptRepl = CL.replaceVarBindingStmt clOpt newBinds
        c = translate clOptRepl

    putStrLn "--- Translating to CLang ---"
    putStrLn $ CL.showCStmt 0 clBase

    putStrLn "\n--- Merging Lambdas ---"
    let (merged, mergedMap) = mergeLambdas c c Map.empty
    putStrLn $ showCStmt 0 mergedMap Map.empty Map.empty merged
    let (cbody, closureEnv, liftenv, funs, defs) = lambdaLift merged
    -- let (cbody, closureEnv, liftenv, funs, defs) = lambdaLift c
    let strFunTypes = getStrFunTypes defs Map.empty
    
    putStrLn "\n--- Printing C ---"
    let imports =   "\n#include <stdbool.h>" ++
                    "\n#include <stdio.h>" ++
                    "\n#include <stdlib.h>" ++
                    "\n#include <stdint.h>" ++
                    libName
    let closureStructs = generateClosureStructs (Map.toList liftenv)
    let funDefs = showFunDefs defs
    let (funPart, mainBody) = splitTopLevel cbody
    let retExpr = findFirstReturn mainBody
    let mainBodyWithoutRet = removeFirstReturn mainBody

    -- print funs
    -- let funImpl = showCStmt 0 Map.empty closureEnv strFunTypes funPart
    -- let mainBodyImpl = showCStmt 1 Map.empty closureEnv strFunTypes mainBodyWithoutRet
    -- let retImpl = showCExpression retExpr Map.empty
    let retImpl = showCExpression retExpr mergedMap
    let mainBodyImpl = showCStmt 1 mergedMap closureEnv strFunTypes mainBodyWithoutRet
    let funImpl = showCStmt 0 mergedMap closureEnv strFunTypes funPart

    let content =
            "\n// imports" ++ imports ++
            "\n// function defitions" ++ funDefs ++
            "\n\n// closure defitions" ++ showCStmt 0 Map.empty Map.empty strFunTypes closureStructs ++
            "\n// function implementations" ++ funImpl ++
            "\n// main\nint main(void) {" ++ mainBodyImpl ++
                    case show (typeRep mainBody) of
                        "Int" -> "\n  printInt("
                        "[Int]" -> "\n  printList("
                        _ -> error "cannot print"
            ++ retImpl ++ ");\n" ++ "  return 0;\n}\n"

    -- writing to file
    let fileName = "outputs/" ++ progName ++ ".c"
    handle <- openFile fileName WriteMode
    hPutStrLn handle content
    hClose handle
    putStrLn $ "Successfully wrote to " ++ fileName
