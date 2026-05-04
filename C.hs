{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- gcc ./outputs/sumListCall_output.c -o ./outputs/sumListCall_output
-- ./outputs/sumListCall_output

{-

-}

module C where

import CLang (indentStr, showProx)
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
import Text.Read (Lexeme(String))
import qualified Data.IntMap as List

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

data ClosureTag n

data CType
    = CTypeRep TypeRep
    | CClosurePtr      -- Closure*  (uniform, no struct id needed)
    | CVoidPtr         -- void*     (for env and untyped returns)
    deriving (Show)

data CArg where
  CArg :: Typeable a => CExpression a -> CArg

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    UnitV :: CValue ()
    FunV  :: (CValue a -> CValue b) -> CValue (a -> b)
    PairV :: CValue a -> CValue b -> CValue (a, b)
    ListV :: [CValue a] -> CValue [a]
    ClosureV :: CValue a

data CExpression a where
    Val :: CValue a -> CExpression a
    Not :: CExpression Bool -> CExpression Bool
    Var :: (Typeable a) => Int -> CExpression a
    LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
    LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
    Ternary :: CExpression Bool -> CExpression a -> CExpression a -> CExpression a
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
    CastExpr :: CType -> CExpression a -> CExpression b  -- (int)(intptr_t)x or (Node*)x
    ApplyClosure :: CExpression a -> CExpression b -> CExpression c  -- apply(f, arg)
    GetEnvField :: Int -> Int -> CType -> CExpression a  -- ((Env_vN*)env)->vM, with type for cast
    CallExpr :: (Typeable a, Typeable b) => CExpression (a -> b) -> CExpression a -> CExpression b

data CStatement a where
    Return :: CExpression a -> CStatement a
    BindExpr :: Typeable a => CExpression a -> Int -> CStatement b -> CStatement b
    Seq :: CStatement a -> CStatement a -> CStatement a
    If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a
    DefFun :: (Typeable b) => CType -> Int -> CParams -> CStatement b -> CStatement b
    DefVar :: Typeable a => Int -> CExpression a -> CStatement b
    UpdateVar :: Typeable a => Int -> CExpression a -> CStatement b
    While :: CExpression Bool -> CStatement a -> CStatement a
    Skip :: CStatement a
    DefClosureStruct :: Int -> CParams -> CStatement a  -- same, but fields are concrete types
    AllocClosure :: Int -> Int -> CParams -> CParams -> CStatement a
    --              structId  implId  directParams  parentEnvParams

type LiftEnv = Map.Map Int CParams
type Lifted a = [CStatement a]

type ClosureReturnEnv = Map.Map Int Int

translateValue :: CL.CValue a -> CValue a
translateValue (CL.IntV x) = IntV x
translateValue (CL.BoolV x) = BoolV x
translateValue CL.UnitV = UnitV
translateValue (CL.PairV x y) = PairV (translateValue x) (translateValue y)
translateValue (CL.FunV f) = FunV (\v -> translateValue (f (translateValueBack v)))
translateValue (CL.ListV x) = ListV (map translateValue x)

translateValueBack :: CValue a -> CL.CValue a
translateValueBack (IntV x) = CL.IntV x
translateValueBack (BoolV x) = CL.BoolV x
translateValueBack UnitV = CL.UnitV
translateValueBack (PairV x y) = CL.PairV (translateValueBack x) (translateValueBack y)
translateValueBack (ListV xs) = CL.ListV (map translateValueBack xs)
translateValueBack (FunV f) = CL.FunV (\v -> translateValueBack (f (translateValue v)))
translateValueBack _ = error $ "Cannot translate back"

translateExpr :: CL.CExpression a -> CExpression a
translateExpr (CL.Val v) = Val (translateValue v)
translateExpr (CL.Not e) = Not (translateExpr e)
translateExpr (CL.Var i) = Var i
translateExpr (CL.LIntOp op e1 e2) = LIntOp op (translateExpr e1) (translateExpr e2)
translateExpr (CL.LCmpOp op e1 e2) = LCmpOp op (translateExpr e1) (translateExpr e2)
translateExpr (CL.Ternary c t f) = Ternary (translateExpr c) (translateExpr t) (translateExpr f)
translateExpr (CL.CallExpr f x) = CallExpr (translateExpr f) (translateExpr x)
translateExpr (CL.Prod a b) = Prod (translateExpr a) (translateExpr b)
translateExpr (CL.Fst p) = Fst (translateExpr p)
translateExpr (CL.Snd p) = Snd (translateExpr p)
translateExpr CL.EmptyList = EmptyList
translateExpr (CL.ConsList h t) = ConsList (translateExpr h) (translateExpr t)
translateExpr (CL.HeadList l) = HeadList (translateExpr l)
translateExpr (CL.TailList l) = TailList (translateExpr l)
translateExpr (CL.IsEmpty l) = IsEmpty (translateExpr l)
translateExpr (CL.IndexList l i) = IndexList (translateExpr l) (translateExpr i)

translate :: CL.CStatement a -> CStatement a
translate CL.Skip = Skip
translate (CL.Return x) = Return (translateExpr x)
translate (CL.DefVar i x) = DefVar i (translateExpr x)
translate (CL.UpdateVar i x) = UpdateVar i (translateExpr x)
translate (CL.While cond x) = While (translateExpr cond) (translate x)
translate (CL.Seq x y) = Seq (translate x) (translate y)
translate (CL.BindExpr x i s) = BindExpr (translateExpr x) i (translate s)
translate (CL.If cond x y) = If (translateExpr cond) (translate x) (translate y)
translate (CL.DefFun tret ifun (ip, tp) body) = DefFun (CTypeRep (typeRep tret)) ifun [CParam ip tp] (translate body)


-- LAMBDA LIFTING

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
freeVarsExpr (Var i) = (Map.singleton i (CParam i (Proxy :: Proxy a)), Map.empty)
freeVarsExpr (Ternary cond thn els) = merge (merge (freeVarsExpr cond) (freeVarsExpr thn)) (freeVarsExpr els)

freeVarsStmt :: Typeable a => CStatement a -> (CParamMap, CParamMap)
freeVarsStmt (BindExpr (x :: CExpression a) i y) =
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
   -- in trace ("\nFree DefFun -> " ++ show (Map.keys actualFree) ++ " | " ++ show (Map.keys bbound) ++ " | " ++ show (Map.keys boundKeys) ++ " \n " ++ showCStmt 0 Map.empty body) $ (actualFree, Map.insert ifun undefined (Map.union bbound boundKeys))
   in (actualFree, Map.insert ifun undefined (Map.union bbound boundKeys))
freeVarsStmt (UpdateVar i (x :: CExpression a)) =
    let (xfree, xbound) = freeVarsExpr x
    in (Map.union (Map.singleton i (CParam i (Proxy :: Proxy a))) xfree, xbound)
freeVarsStmt (DefVar i (x :: CExpression a)) =
    let (xfree, xbound) = freeVarsExpr x
    in (xfree, Map.insert i (CParam i (Proxy :: Proxy a)) xbound)
freeVarsStmt (Return x) = freeVarsExpr x
freeVarsStmt Skip = (Map.empty, Map.empty)
freeVarsStmt _ = (Map.empty, Map.empty)

freeVars :: Typeable a => CStatement a -> CParamMap
freeVars s =
    let (free, bound) = freeVarsStmt s
    in Map.difference free bound

applyArgs :: forall a. Typeable a => CExpression a -> CParams -> CExpression a
applyArgs acc [] = acc
applyArgs acc ((CParam i (Proxy :: Proxy p)) : vs) =
    applyArgs (CallExpr (unsafeCoerce acc) (Var i :: CExpression p)) vs
applyArgs acc (CParamEnv i : vs) =
    applyArgs (unsafeCoerce $ CallExpr
        (unsafeCoerce acc :: CExpression (Int -> Int))
        (Var i :: CExpression Int)) vs
-- applyArgs acc (CParamClosure i x : vs) =
--     applyArgs (CallExpr (unsafeCoerce acc) (Var i)) vs

rewriteExpr :: Int -> LiftEnv -> ClosureReturnEnv -> CExpression a -> CExpression a
rewriteExpr ifun env closureRet expr@(CallExpr _ _) =
    let (func, args) = collectArgs expr
        args' = map (\(CArg a) -> CArg (rewriteExpr ifun env closureRet a)) args
        rebuildCall f as = foldl (\acc (CArg a) ->
            unsafeCoerce $ CallExpr 
                (unsafeCoerce acc :: CExpression (Int -> Int)) 
                (unsafeCoerce a :: CExpression Int))
            f as
    in case func of
        Var f -> trace ("fun " ++ showCExpression func Map.empty ++ " | num args " ++ show (length args) ++ " | ") $
            case Map.lookup f closureRet of
            Just _ -> trace ("IS CLOSURE " ++ showCExpression func Map.empty) $
                -- f is a closure factory, so f(arg1)(arg2) -> apply(f(arg1), arg2)
                -- arg1 goes to the factory, arg2 is passed via apply
                let (factoryArgs, applyArgs') = splitAt 1 args'  -- factory takes 1 arg
                    factoryCall = rebuildCall (Var f :: CExpression (Int -> Int)) factoryArgs
                    in case applyArgs' of
                        [] -> trace ("PArtial " ++ showCExpression func Map.empty) $ unsafeCoerce factoryCall  -- partial application, just the factory call
                        _  -> foldl (\(acc :: CExpression a) (CArg a) ->
                                unsafeCoerce $ CastExpr (CTypeRep (typeRep (Proxy :: Proxy a))) (ApplyClosure (unsafeCoerce acc) (unsafeCoerce a)))
                              (unsafeCoerce factoryCall) applyArgs'
            Nothing -> trace ("Not CLOSURE " ++ showCExpression func Map.empty) $ case Map.lookup f env of
                Just extraVars ->
                    let base = rebuildCall (Var f :: CExpression (Int -> Int)) args'
                    in applyArgs (unsafeCoerce base) extraVars
                Nothing ->
                    unsafeCoerce $ rebuildCall (Var f :: CExpression (Int -> Int)) args'
        _ ->
            let func' = rewriteExpr ifun env closureRet (unsafeCoerce func)
            in unsafeCoerce $ rebuildCall (unsafeCoerce func') args'
    where
    collectArgs :: CExpression a -> (CExpression a, [CArg])
    collectArgs (CallExpr f x) =
        let (f', args) = collectArgs (unsafeCoerce f)
        in (f', args ++ [CArg x])
    collectArgs e = (e, [])
rewriteExpr ifun m closureRet  (Not x) = Not (rewriteExpr ifun m closureRet x)
rewriteExpr ifun m closureRet  (Fst x) = Fst (rewriteExpr ifun m closureRet x)
rewriteExpr ifun m closureRet  (Snd x) = Snd (rewriteExpr ifun m closureRet x)
rewriteExpr ifun m closureRet  (IsEmpty x) = IsEmpty (rewriteExpr ifun m closureRet x)
rewriteExpr ifun m closureRet  (HeadList x) = HeadList (rewriteExpr ifun m closureRet x)
rewriteExpr ifun m closureRet  (TailList x) = TailList (rewriteExpr ifun m closureRet x)
rewriteExpr ifun m closureRet  (IndexList l i) = IndexList (rewriteExpr ifun m closureRet l) i
rewriteExpr ifun m closureRet  (Prod x y) = Prod (rewriteExpr ifun m closureRet x) (rewriteExpr ifun m closureRet y)
rewriteExpr ifun m closureRet  (ConsList l x) = ConsList (rewriteExpr ifun m closureRet l) (rewriteExpr ifun m closureRet x)
rewriteExpr ifun m closureRet  (LIntOp op x y) = LIntOp op (rewriteExpr ifun m closureRet x) (rewriteExpr ifun m closureRet y)
rewriteExpr ifun m closureRet  (LCmpOp op x y) = LCmpOp op (rewriteExpr ifun m closureRet x) (rewriteExpr ifun m closureRet y)
rewriteExpr ifun m closureRet  (Ternary x y z) = Ternary (rewriteExpr ifun m closureRet x) (rewriteExpr ifun m closureRet y) (rewriteExpr ifun m closureRet z)
rewriteExpr ifun m _  ((Var i) :: CExpression a) =
    case Map.lookup ifun m of
        Just extraPs | findParam extraPs i ->
            GetEnvField ifun i (CTypeRep (typeRep (Proxy :: Proxy a)))
        _ -> Var i
        where
            findParam :: CParams -> Int -> Bool
            findParam [] _ = False
            findParam [CParam ip _] toFind = ip == toFind
            findParam (p:rest) toFind = findParam [p] toFind || findParam rest toFind
rewriteExpr _ _ _ x = x

rewriteStmt :: Int -> LiftEnv -> ClosureReturnEnv -> CStatement a -> CStatement a
rewriteStmt ifun m closureRet (BindExpr x i y) = BindExpr (rewriteExpr ifun m closureRet x) i (rewriteStmt ifun m closureRet y)
rewriteStmt ifun m closureRet (Seq x y) = Seq (rewriteStmt ifun m closureRet x) (rewriteStmt ifun m closureRet y)
rewriteStmt ifun m closureRet (If cond x y) = If (rewriteExpr ifun m closureRet cond) (rewriteStmt ifun m closureRet x) (rewriteStmt ifun m closureRet y)
rewriteStmt ifun m closureRet (While cond x) = While (rewriteExpr ifun m closureRet cond) (rewriteStmt ifun m closureRet x)
rewriteStmt _ m closureRet (DefFun tret ifun1 params body) = DefFun tret ifun1 params (rewriteStmt ifun1 m closureRet body)
rewriteStmt ifun m closureRet (UpdateVar i x) = UpdateVar i (rewriteExpr ifun m closureRet x)
rewriteStmt ifun m closureRet (DefVar i x) = DefVar i (rewriteExpr ifun m closureRet x)
rewriteStmt ifun m closureRet (Return x) = Return (rewriteExpr ifun m closureRet x)
rewriteStmt _ _ _ Skip = Skip
rewriteStmt _ _ _ x = x

liftedFunsList :: Lifted a -> [Int]
liftedFunsList [] = []
liftedFunsList [DefFun _ i _ _] = [i]
liftedFunsList (i:is) = liftedFunsList [i] ++ liftedFunsList is


liftStmt :: Int -> LiftEnv -> ClosureReturnEnv -> [Int] -> CStatement a -> (LiftEnv, ClosureReturnEnv, Lifted a, CStatement a)
liftStmt _ env closureRet funs (DefFun tret ifun params body) =
    let freeMapRaw = freeVars (DefFun tret ifun params body)
        freeMap = Map.withoutKeys freeMapRaw (Set.fromList funs)
        extraPs = Map.elems freeMap
        newParams = case extraPs of
            [] -> params
            _  -> CParamEnv ifun : params
        env' = Map.insert ifun extraPs env
        
        closureRet' = 
            case body of
                Seq (DefFun _ ifun1 _ _) (Return (Var ret1)) ->
                    if ifun1 == ret1
                    then Map.insert ifun ifun1 closureRet
                    else closureRet
                _ -> closureRet

        (env'', closureRet'', lifted, body') = liftStmt ifun env' closureRet' (funs ++ [ifun]) body
        
        (body'', closureRet''') =
            case body of
                Seq (DefFun _ ifun1 _ _) (Return (Var ret1)) ->
                    if ifun1 == ret1
                    then let innerExtraPs = extraPs \\ params
                         in (AllocClosure ifun1 ifun1 params innerExtraPs, Map.insert ifun ifun1 closureRet'')
                    else (rewriteStmt ifun env'' closureRet'' body', closureRet'')
                _ -> (rewriteStmt ifun env'' closureRet'' body', closureRet'')
        thisDef = case body of
            Seq (DefFun _ ifun1 _ _) (Return (Var ret1)) ->
                if ifun1 == ret1
                then DefFun CClosurePtr ifun newParams body''
                else DefFun tret ifun newParams body''
            _ -> DefFun tret ifun newParams body''
    in (env'', closureRet''', lifted ++ [thisDef], Skip)
liftStmt fun env closureRet funs (Seq x y) =
    let (env', closureRet',  lx, x') = liftStmt fun env closureRet funs  x
        (env'', closureRet'',  ly, y') = liftStmt fun env' closureRet' funs y
    in (env'', closureRet'', lx ++ ly, Seq x' y')
liftStmt fun env closureRet funs (If cond x y) =
    let (env', closureRet',  lx, x') = liftStmt fun env closureRet funs  x
        (env'', closureRet'',  ly, y') = liftStmt fun env' closureRet' funs y
    in (env'', closureRet'', lx ++ ly, If (rewriteExpr fun env closureRet'' cond) x' y')
liftStmt fun env closureRet funs (While cond x) =
    let (env', closureRet', lx, x') = liftStmt fun env closureRet funs x
    in (env', closureRet', lx, While (rewriteExpr fun env closureRet' cond) x')
liftStmt fun env closureRet funs (BindExpr x i y) =
    let (env', closureRet', ly, y') = liftStmt fun env closureRet funs y
    in (env', closureRet', ly, BindExpr (rewriteExpr fun env closureRet' x) i y')
liftStmt fun env closureRet _ s = (env, closureRet, [], rewriteStmt fun env closureRet s)

lambdaLift :: CStatement a -> (CStatement a, LiftEnv, [CStatement a])
lambdaLift stmt =
    let (env, _, lifted, stmt') = liftStmt 0 Map.empty Map.empty [] stmt
    in (Prelude.foldr Seq stmt' lifted, env, lifted)

generateClosureStructs :: [(Int, CParams)] -> CStatement a
generateClosureStructs [] = Skip
generateClosureStructs [(_, [])] = Skip
generateClosureStructs [(ifun, p)] = DefClosureStruct ifun p
generateClosureStructs (i:is) = Seq (generateClosureStructs [i]) (generateClosureStructs is)



{-
    typedef struct {
        int v2;
    } Env_v4;

    void* v4_impl(void* env, void* v3) {
        return (void*)(intptr_t)((Env_v4*)env->v2 + v0((Node*)v3));
    }

    Closure* v5(int v2) {
        Env_v4* e = malloc(sizeof(Env_v4));
        e->v2 = v2;
        Closure* c = malloc(sizeof(Closure));
        c->env = e;
        c->fn  = (void* (*)(void*, void*))v4_impl;  // cast here once
        return c;
    }

    int v0(Node* v1) {
        return isEmpty(v1) ? 0 : (int)(intptr_t)apply(v5(*(int*)head(v1)), tail(v1));
    }
-}



-- OPTIMISATIONS

-- for a function (int), given the amount of params, check that every call site has at least that many applications
checkCallExpr :: Int -> Int -> CExpression a -> Bool
checkCallExpr fun params expr =
    let (f, args) = collectArgs expr
    in case f of
        Var i | i == fun -> length args >= params
        _ -> True  -- not a call to our function, fine
  where
    collectArgs :: CExpression a -> (CExpression a, [CArg])
    collectArgs (CallExpr f args) =
        let (f', args') = collectArgs (unsafeCoerce f)
        in (f', args' ++ [CArg args])
    collectArgs e = (e, [])

checkCallStmt :: Int -> Int -> CStatement a -> Bool
checkCallStmt fun params stmt = case stmt of
    Return e       -> checkCallExpr fun params e
    Seq x y        -> checkCallStmt fun params x && checkCallStmt fun params y
    If c t e       -> checkCallExpr fun params c &&
                      checkCallStmt fun params t &&
                      checkCallStmt fun params e
    BindExpr e _ s -> checkCallExpr fun params e && checkCallStmt fun params s
    DefFun _ _ _ b -> checkCallStmt fun params b
    While c b      -> checkCallExpr fun params c && checkCallStmt fun params b
    _              -> True

-- retrun merged and map of functions to their new number of params
mergeLambdas :: CStatement a -> Map.Map Int Int -> (CStatement a, Map.Map Int Int)
mergeLambdas (DefFun tret ifun params body) m =
    case body of
        (Seq (DefFun tret1 ifun1 params1 body1) (Return (Var _))) ->
            let newParams = params ++ params1
                canMerge  = checkCallStmt ifun1 (length params1) (unsafeCoerce body1)
            in if canMerge then
                let newDef = unsafeCoerce $ DefFun tret1 ifun newParams (unsafeCoerce body1 :: CStatement Int)
                    newMap    = Map.insert ifun (length newParams) m
                in (unsafeCoerce newDef, newMap)
                else let (body', m') = mergeLambdas body m
                    in (DefFun tret ifun params body', m')
        _ -> let (body', m') = mergeLambdas body m
            in (DefFun tret ifun params body', m')
mergeLambdas (Seq x y) m =
    let (x', m')  = mergeLambdas x m
        (y', m'') = mergeLambdas y m'
    in (Seq x' y', m'')
mergeLambdas stmt m = (stmt, m)

-- SHOW

showProxVar :: String -> TypeRep -> String
showProxVar s p =
    let args = typeRepArgs p
        con  = show (typeRepTyCon p)
    in case (con, args) of
        ("Int",  [])     -> "int " ++ s
        ("Bool", [])     -> "bool " ++ s
        ("()",   [])     -> "void* " ++ s
        ("[]",   [_])     -> "Node* " ++ s
        ("(,)",  [a, _]) -> showProx a ++ "* " ++ s
        ("->",   [a, b]) -> showProx b ++ " (*" ++ s ++ ")(" ++ showProx a ++ ")"
        _                -> show p ++ s

showProxFunc :: String -> CParams -> CType -> String
showProxFunc s params typ =
    case typ of
        CTypeRep p ->
            let args = typeRepArgs p
                con  = show (typeRepTyCon p)
            in case (con, args) of
                ("Int",  [])     -> "int " ++ s ++ "(" ++ showCParams params ++ ")"
                ("Bool", [])     -> "bool " ++ s ++ "(" ++ showCParams params ++ ")"
                ("()",   [])     -> "void* " ++ s ++ "(" ++ showCParams params ++ ")"
                ("[]",   [_])     -> "Node* " ++ s ++ "(" ++ showCParams params ++ ")"
                ("(,)",  [a, _]) -> showProx a ++ "* " ++ s ++ "(" ++ showCParams params ++ ")"
                ("->",   [a, b]) -> showProx b ++ " (*" ++ s ++ "(" ++ showCParams params ++ ")" ++ ")(" ++ showProx a ++ ")"
                _                -> show p ++ s ++ "(" ++ showCParams params ++ ")"
        CClosurePtr -> "Closure* " ++ s ++ "(" ++ showCParams params ++ ")"
        CVoidPtr -> "void* " ++ s ++ "(" ++ showCParams params ++ ")"

showCParams :: CParams -> String
showCParams [] = ""
showCParams [CParam i t] = showProxVar ("v" ++ show i) (typeRep t)
-- showCParams [CParamEnv iclosure] = "Env_v" ++ show iclosure ++ "* env"
showCParams [CParamEnv _] = "void* env"
showCParams (i:is) = showCParams [i] ++ ", " ++ showCParams is

showCValue :: CValue a -> String
showCValue (IntV n)  = show n
showCValue (BoolV b) = show b
showCValue UnitV = "NULL"
showCValue (PairV x y) = "(" ++ showCValue x ++ ", " ++ showCValue y ++ ")"
showCValue (FunV _) = "funv"
showCValue (ListV l) =
  case l of
    [] -> ""
    [h] -> showCValue h
    (h:t) -> showCValue h ++ ", " ++ showCValue (ListV t)
showCValue ClosureV = "env"

showCExpression :: CExpression a -> Map.Map Int Int -> String
showCExpression (Var i) _ = "v" ++ show i
showCExpression (Not x) m = "!" ++ showCExpression x m
showCExpression (LIntOp op x y) m = "(" ++ showCExpression x m ++ " " ++ CL.showBinOp op ++ " " ++ showCExpression y m ++ ")"
showCExpression (LCmpOp op x y) m = "(" ++ showCExpression x m ++ " " ++ CL.showCmpOp op ++ " " ++ showCExpression y m ++ ")"
showCExpression (Val v) _ = showCValue v
showCExpression (CallExpr f arg) m = -- merges together nested calls if I merged together the params earlier
    let (func, args) = collectArgs (CallExpr f arg)
    in case func of
        Var i -> case Map.lookup i m of
            Just n ->
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
  where
    collectArgs :: CExpression a -> (CExpression a, [CArg])
    collectArgs (CallExpr fun1 arg1) =
        let (func, args) = collectArgs (unsafeCoerce fun1)
        in (func, args ++ [CArg arg1])
    collectArgs e = (e, [])
showCExpression (Prod l r) m = "(" ++ showCExpression l m ++ "," ++ showCExpression r m ++ ")"
showCExpression (Fst p) m = showCExpression p m ++ "[0]"
showCExpression (Snd p) m = showCExpression p m ++ "[1]"
showCExpression EmptyList _ = "NULL"
showCExpression (ConsList x l) m = "cons(&(" ++ showProx (typeRep x) ++ "){" ++ showCExpression x m ++ "}, " ++ showCExpression l m ++ ")"
showCExpression (IsEmpty l) m = "isEmpty(" ++ showCExpression l m ++ ")"
showCExpression (HeadList l) m = "*(" ++ CL.showProxList (typeRep l) ++ ")" ++ "head(" ++ showCExpression l m ++ ")"
showCExpression (TailList l) m = "tail(" ++ showCExpression l m ++ ")"
showCExpression (IndexList l i) m = showCExpression l m ++ "[" ++ showCExpression i m ++ "]"
showCExpression (Ternary cond thn els) m = "(" ++ showCExpression cond m ++ ") ? (" ++ showCExpression thn m ++ ") : (" ++ showCExpression els m ++ ")"
showCExpression (CastExpr t x) m = case t of
    CTypeRep p ->
        let con = show (typeRepTyCon p)
        in case con of
            "Int"  -> "(int)(intptr_t)" ++ showCExpression x m
            "Bool" -> "(bool)" ++ showCExpression x m
            "[]"   -> "(Node*)" ++ showCExpression x m
            _      -> "(void*)" ++ showCExpression x m
    CVoidPtr    -> "(void*)" ++ showCExpression x m
    CClosurePtr -> "(Closure*)" ++ showCExpression x m
showCExpression (GetEnvField structId fieldId _) _ =
    "((Env_v" ++ show structId ++ "*)env)->v" ++ show fieldId
showCExpression (ApplyClosure f arg) m =
    "apply(" ++ showCExpression f m ++ ", " ++ showCExpression arg m ++ ")"

    -- CastExpr :: CType -> CExpression a -> CExpression b  -- (int)(intptr_t)x or (Node*)x
    -- GetEnvField :: Int -> CType -> CExpression a  -- ((Env_vN*)env)->vM, with type for cast

showCStmt :: Int -> Map.Map Int Int -> CStatement a -> String
showCStmt indent m (UpdateVar i x) = "\n" ++ indentStr indent ++ "v" ++ show i ++ " = " ++ showCExpression x m ++ ";"
showCStmt indent m (If cond t f) =
    "\n" ++ indentStr indent ++ "if " ++ showCExpression cond m ++ " {"
    ++  showCStmt (indent + 1) m t
    ++ "\n" ++ indentStr indent ++ "} else {"
    ++ showCStmt (indent + 1) m f
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent m (While cond body) =
    "\n" ++ indentStr indent ++ "while " ++ showCExpression cond m ++ " {"
    ++ showCStmt (indent + 1) m body
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent m (BindExpr x i y) =
    "\n" ++ indentStr indent ++ "let v" ++ show i ++ " = " ++ showCExpression x m ++ " in"
    ++ showCStmt (indent + 1) m y
showCStmt indent m (Seq x y) =
    showCStmt indent m x ++ showCStmt indent m y
showCStmt indent m (DefFun prox ifun params body) =
    "\n" ++ indentStr indent ++ showProxFunc ("v" ++ show ifun) params prox ++ " {"
    ++ showCStmt (indent + 1) m body
    ++ "\n" ++ indentStr indent ++ "}\n"
showCStmt indent m (DefVar i f) =  "\n" ++ indentStr indent ++ showProxVar ("v" ++ show i) (typeRep f) ++ " = " ++ showCExpression f m ++ ";"
showCStmt indent m (Return x) =  "\n" ++ indentStr indent ++ "return " ++ showCExpression x m ++ ";"
showCStmt indent _ (DefClosureStruct ifun p) =
    "\n" ++ indentStr indent ++ "typedef struct {\n"
    ++ showVars p
    ++ "} Env_v" ++ show ifun ++ ";\n"
    where
        showVars :: CParams -> String
        showVars [] = ""
        showVars [CParam ip tp] = "    " ++ showProxVar ("v" ++ show ip) (typeRep tp) ++ ";\n"
        showVars (first:rest) = showVars [first] ++ showVars rest
showCStmt indent _ (AllocClosure structId implId params parentParams) =
    "\n" ++ indentStr indent ++ "Env_v" ++ show structId ++ "* env" ++ show structId ++ " = malloc(sizeof(Env_v" ++ show structId ++ "));"
    ++ showVars params
    ++ showParentParams parentParams
    ++ "\n" ++ indentStr indent ++ "Closure* c = malloc(sizeof(Closure));"
    ++ "\n" ++ indentStr indent ++ "c->env = env" ++ show structId ++ ";"
    ++ "\n" ++ indentStr indent ++ "c->fn = (void* (*)(void*, void*))v" ++ show implId ++ ";"
    ++ "\n" ++ indentStr indent ++ "return c;"
    where
        showVars :: CParams -> String
        showVars [] = ""
        showVars [CParam ip _] = "\n" ++ indentStr indent ++ "env" ++ show structId ++ "->v" ++ show ip ++ " = v" ++ show ip ++ ";"
        showVars [CParamEnv ip] = "\n" ++ indentStr indent ++ "env" ++ show structId ++ "->v" ++ show ip ++ " = v" ++ show ip ++ ";"
        showVars (first:rest) = showVars [first] ++ showVars rest
        
        showParentParams :: CParams -> String
        showParentParams [] = ""
        showParentParams (CParam ip _ : rest) =
            "\n" ++ indentStr indent ++ "env" ++ show structId ++ "->v" ++ show ip 
            ++ " = ((Env_v" ++ show structId ++ "*)env)->v" ++ show ip ++ ";"
            ++ showParentParams rest
        showParentParams (_ : rest) = showParentParams rest
showCStmt _ _ Skip = ""

-- listPreamble :: String
-- listPreamble =
--     "\n// List Definitions" ++
--     "\ntypedef struct Node {" ++
--     "\n    void* head;" ++
--     "\n    struct Node* tail;" ++
--     "\n} Node;\n" ++
--     "\nNode* cons(void* head, Node* tail) {" ++
--     "\n    Node* node = malloc(sizeof(Node));" ++
--     "\n    node->head = head;" ++
--     "\n    node->tail = tail;" ++
--     "\n    return node;" ++ "\n}" ++ "\n" ++
--     "\nint isEmpty(Node* xs) {" ++
--     "\n    return xs == NULL;" ++
--     "\n}\n" ++
--     "\nvoid* head(Node* xs) {" ++
--     "\n    return xs->head;" ++
--     "\n}\n" ++
--     "\nNode* tail(Node* xs) {" ++
--     "\n    return xs->tail;\n}\n"

-- usesListExpr :: CExpression a -> Bool
-- usesListExpr EmptyList = True
-- usesListExpr IsEmpty{} = True
-- usesListExpr ConsList{} = True
-- usesListExpr HeadList{} = True
-- usesListExpr TailList{} = True
-- usesListExpr IndexList{} = True
-- usesListExpr (Not x) = usesListExpr x
-- usesListExpr (Fst x) = usesListExpr x
-- usesListExpr (Snd x) = usesListExpr x
-- usesListExpr (Prod x y) = usesListExpr x || usesListExpr y
-- usesListExpr (CallExpr f x) = usesListExpr f || usesListExpr x
-- usesListExpr (LCmpOp _ x y) = usesListExpr x || usesListExpr y
-- usesListExpr (LIntOp _ x y) = usesListExpr x || usesListExpr y
-- usesListExpr (Ternary x y z) = usesListExpr x || usesListExpr y || usesListExpr z
-- usesListExpr (Val (CL.ListV _)) = True
-- usesListExpr _ = False

-- usesList :: CStatement a -> Bool
-- usesList (Return x) = usesListExpr x
-- usesList (BindExpr x _ y) = usesListExpr x || usesList y
-- usesList (Seq x y) = usesList x || usesList y
-- usesList (If x y z) = usesListExpr x || usesList y || usesList z
-- usesList (While x y) = usesListExpr x || usesList y
-- usesList (UpdateVar _ x) = usesListExpr x
-- usesList (DefVar _ x) = usesListExpr x
-- usesList (DefFun _ _ _ y) = usesList y
-- usesList Skip = False

findFirstReturn :: CStatement a -> CExpression a
findFirstReturn (Return x) = x
findFirstReturn (Seq x y) =
    case x of
        (Return i) -> i
        _ -> findFirstReturn y
findFirstReturn (BindExpr _ _ y) = findFirstReturn y
findFirstReturn _ = error "no return"

removeFirstReturn :: CStatement a -> CStatement a
removeFirstReturn (Return _) = Skip
removeFirstReturn (Seq (Return _) y) = Seq Skip y
removeFirstReturn (Seq x (Return _)) = Seq x Skip
removeFirstReturn (Seq x y) = Seq (removeFirstReturn x) (removeFirstReturn y)
removeFirstReturn x = x

makeFunDefs :: [CStatement a] -> String
makeFunDefs [] = ""
makeFunDefs [DefFun tret ifun params _] = "\n" ++ showProxFunc ("v" ++ show ifun) params tret ++ ";"
makeFunDefs (i:is) = makeFunDefs[i] ++ makeFunDefs is

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

main :: IO ()
main = do
    let progName = "mapListCall"
    let (nl, c') = NL.translate 0 AL.mapListCall
        (cl, _) = runState (CL.translate nl) c'
        c = translate cl

    putStrLn "--- Translating to CLang ---"
    putStrLn $ CL.showCStmt 0 cl

    -- putStrLn "\n--- Merging Lambdas ---"
    -- let (merged, mergedMap) = mergeLambdas c Map.empty
    -- putStrLn $ showCStmt 0 mergedMap merged 

    putStrLn "\n--- Printing C ---"
    let (cbody, liftenv, defs) = lambdaLift c
    let imports = "// imports" ++
                "\n#include <stdbool.h>" ++
                "\n#include <stdio.h>" ++
                "\n#include <stdlib.h>" ++
                "\n#include <stdint.h>" ++
                "\n#include \"listLib.c\"\n"
    -- putStrLn $ showLiftEnv (Map.toList liftenv)
    let closureStructs = generateClosureStructs (Map.toList liftenv)
    let cbody' = Seq closureStructs cbody
    let funDefs = makeFunDefs defs
    let ret = findFirstReturn cbody'
    let bodyWithoutRet = removeFirstReturn cbody'
    let showBodyWithoutRet = showCStmt 0 Map.empty bodyWithoutRet
    let body = showBodyWithoutRet ++ "\nint main(void) {\n" ++
                    "  printf(\"%d\\n\", " ++ showCExpression ret Map.empty ++ ");\n" ++
                    "  return 0;\n}\n"
    let content = imports ++ "\n// Function Definitions" ++ funDefs ++ "\n\n// Compiled Program" ++ body

    -- writing to file
    let fileName = "outputs/" ++ progName ++ "_output.c"
    handle <- openFile fileName WriteMode
    hPutStrLn handle content
    hClose handle
    putStrLn $ "Successfully wrote to " ++ fileName