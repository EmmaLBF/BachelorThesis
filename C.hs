{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}


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
import Data.Maybe (fromJust)

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
    AllocClosure :: Int -> Int -> Int -> CParams -> CParams -> CStatement a
    --              structId  implId  directParams  parentEnvParams

type LiftEnv = Map.Map Int CParams
type Lifted a = [CStatement a]

type ClosureReturnEnv = Map.Map Int Int

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
-- freeVarsStmt (DefClosureStruct x y) = freeVarsExpr x
-- freeVarsStmt (AllocClosure x y z m n) = freeVarsExpr x
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

-- new type to carry hoisted allocations up
type Hoisted a = [CStatement a]
type FunTypes = Map.Map Int CType

step :: CExpression x -> CArg -> CExpression y
step acc (CArg a) =
  unsafeCoerce $ CallExpr
    (unsafeCoerce acc :: CExpression (Int -> Int))
    (unsafeCoerce a   :: CExpression Int)

rebuildCall :: CExpression a -> [CArg] -> CExpression a
rebuildCall = foldl step

showCArgs :: [CArg] -> String
showCArgs [] = ""
showCArgs (CArg x : rest) = "CArg " ++ showCExpression x Map.empty ++ "\n" ++  showCArgs rest

hoistClosureAllocs :: Int -> LiftEnv -> ClosureReturnEnv -> FunTypes -> CExpression a -> (Hoisted b, CExpression a)
hoistClosureAllocs ifun env closureRet funs (expr@(CallExpr _ _) :: CExpression a) =
    let (func, args) = collectArgs expr
        -- hoist from all args first
        (argAllocs, args') = unzip $ map (\(CArg a) ->
            let (allocs, a') = hoistClosureAllocs ifun env closureRet funs a
            in (allocs, CArg a')) args
        allArgAllocs = concat argAllocs
    in case func of
        Var f ->
            trace ("Varhoist " ++ show f ++ " | ifun " ++ show ifun ++ " | closureRet keys=" ++ show (Map.keys closureRet)) $
            case Map.lookup f closureRet of
                Just _ ->
                    let ownExtraPs    = Map.findWithDefault [] f env
                        parentExtraPs = Map.findWithDefault [] ifun env
                        directPs  = ownExtraPs \\ parentExtraPs
                        parentPs  = ownExtraPs `intersect` parentExtraPs
                        (factoryArgs, applyArgs') = splitAt (length directPs) args'

                    in trace ("   ownExtraPs " ++ showCParams ownExtraPs ++ "\n   parentPs " ++ showCParams parentPs) $
                    if null ownExtraPs
                        -- no captured vars: plain C function, call directly, apply rest
                        then
                            let factoryCall = rebuildCall (Var f :: CExpression (Int -> Int)) factoryArgs
                                applied =
                                    case applyArgs' of
                                        [] -> unsafeCoerce factoryCall
                                        (CArg first : rest) ->
                                            case Map.lookup f funs of
                                                Just t ->
                                                    let firstApplied = unsafeCoerce $ CallExpr
                                                            (unsafeCoerce factoryCall :: CExpression (Int -> Int))
                                                            (unsafeCoerce first       :: CExpression Int)
                                                        restApplied =
                                                            foldl (\acc (CArg a) ->
                                                                unsafeCoerce $ CastExpr t (ApplyClosure (unsafeCoerce acc) (unsafeCoerce a)))
                                                                firstApplied
                                                                rest
                                                    -- let firstApplied = unsafeCoerce $ CastExpr t
                                                    --         (ApplyClosure
                                                    --             (unsafeCoerce factoryCall)
                                                    --             (unsafeCoerce first))
                                                    --     restApplied =
                                                    --         foldl (\acc (CArg a) ->
                                                    --             unsafeCoerce $ CastExpr t
                                                    --                 (ApplyClosure (unsafeCoerce acc) (unsafeCoerce a)))
                                                    --             firstApplied
                                                    --             rest
                                                    in restApplied
                                                Nothing -> error "no type"
                            in
                                trace ("   factoryCall " ++ showCExpression factoryCall Map.empty ++ "\n ARGS " ++ showCArgs applyArgs' ++ "    Expr " ++ showCExpression expr Map.empty ++ "\n")
                                (allArgAllocs, unsafeCoerce applied)
                        -- has captured vars: needs AllocClosure, then apply through c
                        else
                            case Map.lookup f funs of
                                Just t ->
                                    let alloc      = AllocClosure f f ifun directPs parentPs
                                        closureVar = unsafeCoerce (Val ClosureV :: CExpression Int)
                                        applied = foldl (\acc (CArg a) ->
                                                        unsafeCoerce $ CastExpr CClosurePtr (ApplyClosure (unsafeCoerce acc) (unsafeCoerce a))) closureVar applyArgs'
                                        final = unsafeCoerce $ CastExpr t applied
                                    in (allArgAllocs ++ [alloc], unsafeCoerce final)


                                Nothing -> error "no type"
                    -- if null ownExtraPs
                    -- then
                    --     let factoryCall = rebuildCall (Var f :: CExpression (Int -> Int)) factoryArgs
                    --         applied =
                    --             case applyArgs' of
                    --                 [] -> unsafeCoerce factoryCall
                    --                 _ ->
                    --                     case Map.lookup f funs of
                    --                         Just t ->
                    --                             foldl (\acc (CArg a) ->
                    --                                 unsafeCoerce $ CastExpr t
                    --                                     (ApplyClosure (unsafeCoerce acc) (unsafeCoerce a)))
                    --                                 (unsafeCoerce factoryCall)
                    --                                 applyArgs'
                    --                         Nothing -> error "no type"
                    --     in (allArgAllocs, unsafeCoerce applied)
                    -- if null ownExtraPs
                    -- then
                    --     let factoryCall = rebuildCall (Var f :: CExpression (Int -> Int)) args'  -- use all args'
                    --         applied = case Map.lookup f funs of
                    --                     Just t  -> unsafeCoerce $ CastExpr t factoryCall
                    --                     Nothing -> unsafeCoerce factoryCall
                    --     in (allArgAllocs, unsafeCoerce applied)
                    -- if null ownExtraPs
                    -- then
                    --     let factoryCall = rebuildCall (Var f :: CExpression (Int -> Int)) args'
                    --         applied = unsafeCoerce $ CastExpr CClosurePtr (unsafeCoerce factoryCall)
                    --     in (allArgAllocs, unsafeCoerce applied)
                    --  else
                        -- case Map.lookup f funs of
                        --     Just t ->
                        --         let alloc      = AllocClosure f f ifun directPs parentPs
                        --             closureVar = unsafeCoerce (Val ClosureV :: CExpression Int)
                        --             applied = foldl (\acc (CArg a) ->
                        --                             unsafeCoerce $ CastExpr CClosurePtr (ApplyClosure (unsafeCoerce acc) (unsafeCoerce a))) closureVar applyArgs'
                        --             final = unsafeCoerce $ CastExpr t applied
                        --         in (allArgAllocs ++ [alloc], unsafeCoerce final)


                        --     Nothing -> error "no type"
                Nothing ->
                    let rebuilt = foldl (\acc (CArg a) ->
                                    unsafeCoerce $ CallExpr
                                        (unsafeCoerce acc :: CExpression (Int -> Int))
                                        (unsafeCoerce a :: CExpression Int))
                                    (Var f :: CExpression (Int -> Int)) args'
                    in (allArgAllocs, unsafeCoerce rebuilt)
        _ ->
            -- trace ("Not varhoist " ++ showCExpression x Map.empty) $
            let rebuilt = foldl (\acc (CArg a) ->
                            unsafeCoerce $ CallExpr
                                (unsafeCoerce acc :: CExpression (Int -> Int))
                                (unsafeCoerce a :: CExpression Int))
                          (unsafeCoerce func) args'
            in (allArgAllocs, unsafeCoerce rebuilt)
  where
    collectArgs :: CExpression a -> (CExpression a, [CArg])
    collectArgs (CallExpr f x) =
        let (f', as) = collectArgs (unsafeCoerce f)
        in (f', as ++ [CArg x])
    collectArgs e = (e, [])
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
hoistClosureAllocs _ _ _ _ x = ([], x)

rewriteExpr :: Int -> LiftEnv -> ClosureReturnEnv -> CExpression a -> CExpression a
rewriteExpr ifun env closureRet expr@(CallExpr _ _) =
    let (func, args) = collectArgs expr
        args' = map (\(CArg a) -> CArg (rewriteExpr ifun env closureRet a)) args
    in case func of
        -- Var f ->
        --     -- trace ("Var " ++ show f) $
        --         case Map.lookup f env of -- add extra vars to funcs with env
        --             Just extraVars | Map.notMember f closureRet ->
        --                 let base = rebuildCall (Var f :: CExpression (Int -> Int)) args'
        --                 in applyArgs (unsafeCoerce base) extraVars
        --             _ -> -- check if f itself needs to become GetEnvField
        --                 let f' = rewriteExpr ifun env closureRet (Var f :: CExpression (Int -> Int))
        --                 in unsafeCoerce $ rebuildCall f' args'
        -- _ -> let func' = rewriteExpr ifun env closureRet func
        --      in unsafeCoerce $ rebuildCall func' args'
        Var f ->
            -- trace ("Var " ++ show f) $
                case Map.lookup f env of -- add extra vars to funcs with env
                    Just extraVars | Map.notMember f closureRet ->
                        let base = rebuildCall (Var f :: CExpression (Int -> Int)) args'
                            -- rewrite each extra var so that captured vars become GetEnvField
                            rewrittenExtras = map (\(CParam i _) -> CArg (rewriteExpr ifun env closureRet (Var i :: CExpression Int))) extraVars
                        in unsafeCoerce $ rebuildCall (unsafeCoerce base) rewrittenExtras
                    _ -> -- check if f itself needs to become GetEnvField
                        let f' = rewriteExpr ifun env closureRet (Var f :: CExpression (Int -> Int))
                        in unsafeCoerce $ rebuildCall f' args'
        _ -> let func' = rewriteExpr ifun env closureRet func
             in unsafeCoerce $ rebuildCall func' args'
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

rewriteStmt :: Int -> LiftEnv -> ClosureReturnEnv -> FunTypes -> CStatement a -> CStatement a
rewriteStmt ifun m closureRet funs (Seq x y) = Seq (rewriteStmt ifun m closureRet funs x) (rewriteStmt ifun m closureRet funs y)
rewriteStmt ifun m closureRet funs (While cond x) = While (rewriteExpr ifun m closureRet cond) (rewriteStmt ifun m closureRet funs x)
rewriteStmt _ m closureRet funs (DefFun tret ifun1 params body) = DefFun tret ifun1 params (rewriteStmt ifun1 m closureRet funs body)
rewriteStmt ifun m closureRet _ (DefVar i x) = DefVar i (rewriteExpr ifun m closureRet x)
rewriteStmt ifun env closureRet funs (Return x) =
    let x' = rewriteExpr ifun env closureRet x
    in case x' of
        Var f -> case Map.lookup f closureRet of
                    Just _ ->
                        let ownExtraPs = Map.findWithDefault [] f env
                        in if null ownExtraPs
                        then Return x'   -- no closure needed
                        else
                            let parentExtraPs = Map.findWithDefault [] ifun env
                                directPs  = ownExtraPs \\ parentExtraPs
                                parentPs  = ownExtraPs `intersect` parentExtraPs
                                alloc = AllocClosure f f ifun directPs parentPs
                            in Seq (unsafeCoerce alloc) (Return (unsafeCoerce (Val ClosureV)))
                    Nothing -> Return x'
        _ ->
            let (allocs, x'') = hoistClosureAllocs ifun env closureRet funs x'
            in foldr (Seq . unsafeCoerce) (Return x'') allocs
rewriteStmt ifun env closureRet funs (BindExpr x i y) =
    let x' = rewriteExpr ifun env closureRet x
        (allocs, x'') = hoistClosureAllocs ifun env closureRet funs x'
        y' = rewriteStmt ifun env closureRet funs y
    in foldr (Seq . unsafeCoerce) (BindExpr x'' i y') allocs
-- rewriteStmt ifun env closureRet funs (BindExpr x i y) =
--     let x' = rewriteExpr ifun env closureRet x
--         (allocs, x'') = hoistClosureAllocs ifun env closureRet funs x'
--         closureRet' = case getTopFunc x' of
--                         Just f | trace ("lift -> " ++ show f) $ Map.member f closureRet -> Map.insert i i closureRet
--                         _ -> closureRet
--         y' = rewriteStmt ifun env closureRet' funs y
--     in foldr (Seq . unsafeCoerce) (BindExpr x'' i y') allocs
--   where
--     getTopFunc :: CExpression a -> Maybe Int
--     getTopFunc (CallExpr f _) = getTopFunc (unsafeCoerce f)
--     getTopFunc (Var j)        = Just j
--     getTopFunc _              = Nothing
rewriteStmt ifun env closureRet funs (If cond t f) =
    let cond' = rewriteExpr ifun env closureRet cond
        (allocs, cond'') = hoistClosureAllocs ifun env closureRet funs cond'
    in foldr (Seq . unsafeCoerce) (If cond'' (rewriteStmt ifun env closureRet funs t)
                            (rewriteStmt ifun env closureRet funs f)) allocs
rewriteStmt ifun env closureRet funs (UpdateVar i x) =
    let x' = rewriteExpr ifun env closureRet x
        (allocs, x'') = hoistClosureAllocs ifun env closureRet funs x'
    in foldr (Seq . unsafeCoerce) (UpdateVar i x'') allocs
rewriteStmt _ _ _ _ Skip = Skip
rewriteStmt _ _ _ _ x = x

showListStmt :: [CStatement a] -> String
showListStmt = concatMap (showCStmt 0 Map.empty Map.empty)

liftedFunsList :: Lifted a -> [Int]
liftedFunsList [] = []
liftedFunsList [DefFun _ i _ _] = [i]
liftedFunsList (i:is) = liftedFunsList [i] ++ liftedFunsList is


-- Extract the id of the inner function/closure that a DefFun body returns,
-- whether it came from a lambda (DefFun pattern) or an Apply (BindExpr pattern).
--   Seq (DefFun _ ifun1 _ _) (Return (Var ifun1))   -- lambda case
--   Seq (BindExpr (CallExpr ...) ret1 _) (Return (Var ret1))  -- apply case
getInnerFunId :: CStatement a -> Maybe Int
getInnerFunId (Seq (DefFun _ ifun1 _ _) (Return (Var ret1)))
    | ifun1 == ret1 = Just ifun1
getInnerFunId (Seq (BindExpr _ ret1 _) (Return (Var ret2)))
    | ret1 == ret2 = Just ret1
getInnerFunId _ = Nothing

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
                                CTypeRep t ->
                                    let args = typeRepArgs t
                                        con = show (typeRepTyCon t)
                                    in case (con, args) of
                                        ("->",   [_, b]) -> Map.insert ifun (CTypeRep b) funs
                                        _                -> Map.insert ifun tret funs
                                _ -> Map.insert ifun tret funs
                    (env'', closureRet'', funs'', lifted, body') = liftStmt ifun env' closureRet' funs' body
                    (body'', closureRet''') =   
                        let innerExtraPs = Map.findWithDefault [] ifun1 env''
                            paramIds = map paramId params
                            directPs = filter (\p -> paramId p `elem` paramIds) innerExtraPs
                            parentPs = innerExtraPs \\ directPs
                        in if null innerExtraPs
                            then (body', closureRet'')
                            else (Seq (AllocClosure ifun1 ifun1 ifun directPs parentPs)
                                    (Return (Val ClosureV)), Map.insert ifun ifun1 closureRet'')
                    thisDef = 
                        let innerExtraPs = Map.findWithDefault [] ifun1 env''
                        in if null innerExtraPs
                            then DefFun tret ifun newParams body''
                            else DefFun CClosurePtr ifun newParams body''
                in (env'', closureRet''', funs'', lifted ++ [thisDef], Skip)
            Nothing -> 
                let funs' = Map.insert ifun tret funs
                    (env'', closureRet'', funs'', lifted, body') = liftStmt ifun env' closureRet funs' body
                    thisDef = DefFun tret ifun newParams body'
                in (env'', closureRet'', funs'', lifted ++ [thisDef], Skip)
        where
            paramId (CParam i _)  = i
            paramId (CParamEnv i) = i





-- liftStmt _ env closureRet funs (DefFun tret ifun params body) =
--     let freeMapRaw = freeVars (DefFun tret ifun params body)
--         freeMap    = Map.withoutKeys freeMapRaw (Map.keysSet funs)
--         extraPs    = Map.elems freeMap
--         newParams  = case extraPs of
--             [] -> params
--             _  -> CParamEnv ifun : params
--         env'       = Map.insert ifun extraPs env
--         closureRet' = case body of
--             Seq (DefFun _ ifun1 _ _) (Return (Var ret1)) ->
--                 if ifun1 == ret1
--                 then let innerExtraPs = Map.findWithDefault [] ifun1 env''
--                     -- only mark as closure-returning if it actually captures something
--                     in if null innerExtraPs then closureRet
--                         else Map.insert ifun ifun1 closureRet
--                 else closureRet
--             _ -> closureRet
--         funs' = case body of
--             Seq (DefFun _ ifun1 _ _) (Return (Var ret1)) ->
--                 if ifun1 == ret1 then
--                     case tret of
--                         CTypeRep t ->   let args = typeRepArgs t
--                                             con  = show (typeRepTyCon t)
--                                         in case (con, args) of
--                                             ("->",   [_, b]) -> Map.insert ifun (CTypeRep b) funs
--                                             _                -> Map.insert ifun tret funs
--                         _ -> Map.insert ifun tret funs
--                 else Map.insert ifun tret funs
--             _ -> Map.insert ifun tret funs
--         (env'', closureRet'', funs'', lifted, body') = liftStmt ifun env' closureRet' funs' body
--         (body'', closureRet''') =
--             case body of
--                 Seq (DefFun tret1 ifun1 params1 body1) (Return (Var ret1)) ->
--                     if ifun1 == ret1
--                     then
--                         let freeMapRaw1 = freeVars (DefFun tret1 ifun1 params1 body1)
--                             freeMap1    = Map.withoutKeys freeMapRaw1 (Map.keysSet funs'')
--                             innerExtraPs = Map.elems freeMap1
--                             -- what ifun can supply directly from its own params
--                             paramIds = map paramId params
--                             -- what comes from ifun's own params (direct)
--                             directPs = filter (\p -> paramId p `elem` paramIds) innerExtraPs
--                             -- what must be threaded from ifun's parent env
--                             parentPs = innerExtraPs \\ directPs
--                         in 
--                             -- trace ("\nTRACE | fun" ++ show ifun ++ " | paramIds " ++ show paramIds ++ " | directPs " ++ showCParams directPs ++ " | parentPs " ++ showCParams parentPs 
--                             --         ++ " | inner " ++ showCParams innerExtraPs ++ "\n | freeRaw " ++ showParamMap (Map.toList freeMapRaw1)
--                             --         ++ "\n | free " ++ showParamMap (Map.toList freeMap1)) $
--                             if null innerExtraPs
--                             then (body', closureRet'')
--                             else (Seq (AllocClosure ifun1 ifun1 ifun directPs parentPs)
--                                     (Return (Val ClosureV)),
--                                 Map.insert ifun ifun1 closureRet'')


--                         -- let innerExtraPs = extraPs \\ params
--                         -- in (Seq (AllocClosure ifun1 ifun1 ifun params innerExtraPs) (Return (Val ClosureV)), Map.insert ifun ifun1 closureRet'')
--                     else (body', closureRet'')
--                 _ -> (body', closureRet'')
--         thisDef = case body of
--             Seq (DefFun _ ifun1 _ _) (Return (Var ret1)) ->
--                 if ifun1 == ret1
--                 then let innerExtraPs = Map.findWithDefault [] ifun1 env''
--                     in if null innerExtraPs
--                         then DefFun tret ifun newParams body''   -- no closure, keep original return type
--                         else DefFun CClosurePtr ifun newParams body''
--                 else DefFun tret ifun newParams body''
--             _ -> DefFun tret ifun newParams body''
--     in (env'', closureRet''', funs'', lifted ++ [thisDef], Skip)
--     where
--     paramId (CParam i _)  = i
--     paramId (CParamEnv i) = i
liftStmt fun env closureRet funs (Seq x y) =
    let (env', closureRet', funs',  lx, x') = liftStmt fun env closureRet funs  x
        (env'', closureRet'', funs'',  ly, y') = liftStmt fun env' closureRet' funs' y
    in (env'', closureRet'', funs'', lx ++ ly, Seq x' y')
liftStmt fun env closureRet funs (If cond x y) =
    let (env', closureRet', funs',  lx, x') = liftStmt fun env closureRet funs  x
        (env'', closureRet'', funs'',  ly, y') = liftStmt fun env' closureRet' funs' y
    in (env'', closureRet'', funs'', lx ++ ly, If (rewriteExpr fun env closureRet'' cond) x' y')
liftStmt fun env closureRet funs (While cond x) =
    let (env', closureRet', funs', lx, x') = liftStmt fun env closureRet funs x
    in (env', closureRet', funs', lx, While (rewriteExpr fun env closureRet' cond) x')
-- liftStmt fun env closureRet funs (BindExpr x i y) =
--     let x' = trace ("\nLIFT bind " ++ show i ++ " -> " ++  showCExpression x Map.empty) $
--             rewriteExpr fun env closureRet x
--         -- (allocs, x'') = hoistClosureAllocs fun env closureRet funs x'
--         closureRet' = trace ("\nLIFT closure " ++ show i ++ " -> " ++  showCExpression x' Map.empty) $
--                         case x' of
--                         Val ClosureV -> Map.insert i i closureRet
--                         _            -> closureRet
--     in liftStmt fun env closureRet' funs 
liftStmt fun env closureRet funs (BindExpr x i y) =
    let closureRet' = case getTopFunc x of
                        Just f | Map.member f closureRet -> Map.insert i i closureRet
                        _ -> closureRet
        funs' = case getTopFunc x of
                    Just f | Map.member f funs -> Map.insert i (fromJust (Map.lookup f funs)) funs
                    _ -> funs
        (env', closureRet'', funs'', ly, y') = liftStmt fun env closureRet' funs' y
        x' = rewriteExpr fun env' closureRet'' x
        (allocs, x'') = hoistClosureAllocs fun env' closureRet'' (Map.union funs funs'') x'
    in (env', closureRet'', funs'', ly, foldr (Seq . unsafeCoerce) (BindExpr x'' i y') allocs)
  where
    getTopFunc :: CExpression a -> Maybe Int
    getTopFunc (CallExpr f _) = getTopFunc (unsafeCoerce f)
    getTopFunc (Var j)        = Just j
    getTopFunc _              = Nothing
liftStmt fun env closureRet funs s = (env, closureRet, funs, [], rewriteStmt fun env closureRet funs s)

lambdaLift :: CStatement a -> (CStatement a, ClosureReturnEnv, LiftEnv, FunTypes, [CStatement a])
lambdaLift stmt =
    let (env, closureRet, funs, lifted, stmt') = liftStmt (-1) Map.empty Map.empty Map.empty stmt
        -- stmt'' = rewriteStmt (-1) env closureRet funs stmt'
    in (Prelude.foldr Seq stmt' lifted, closureRet, env, funs, lifted)

generateClosureStructs :: [(Int, CParams)] -> CStatement a
generateClosureStructs [] = Skip
generateClosureStructs [(_, [])] = Skip
generateClosureStructs [(ifun, p)] = DefClosureStruct ifun p
generateClosureStructs (i:is) = Seq (generateClosureStructs [i]) (generateClosureStructs is)


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
        ("(,)",  [_, _]) -> "Pair* " ++ s
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
                ("(,)",  [_, _]) -> "Pair* " ++ s ++ "(" ++ showCParams params ++ ")"
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
showCValue ClosureV = "c"

showCExpression :: CExpression a -> Map.Map Int Int -> String
showCExpression (Var i) m =
    case Map.lookup i m of  -- use a closureRetMap passed alongside mergedMap
        Just _ -> "c_v" ++ show i
        Nothing -> "v" ++ show i
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
showCExpression (Prod (l :: CExpression a) (r :: CExpression b)) m = 
    let makefun1 = case show (typeRep (Proxy :: Proxy a)) of
                    "Int" -> "mk_int"
                    "Bool" -> "mk_bool"
                    _ -> "mk_int"
        makefun2 = case show (typeRep (Proxy :: Proxy b)) of
                    "Int" -> "mk_int"
                    "Bool" -> "mk_bool"
                    _ -> "mk_int"
    in "mk_pair(" ++ makefun1 ++ "(" ++ showCExpression l m ++ "), " ++ makefun2 ++ "(" ++ showCExpression r m ++ "))"
showCExpression (Fst (p ::CExpression (a,b))) m = 
    let retType = typeRep (Proxy :: Proxy a)
    in "*(" ++ showProx retType ++ "*)fst((Pair*)" ++ showCExpression p m ++ ")"
showCExpression (Snd (p ::CExpression (a,b))) m = 
    let retType = typeRep (Proxy :: Proxy b)
    in "*(" ++ showProx retType ++ "*)snd((Pair*)" ++ showCExpression p m ++ ")"
showCExpression EmptyList _ = "NULL"
showCExpression (ConsList x l) m =
    let makefun = case show (typeRep x) of
                    "Int" -> "mk_int"
                    "Bool" -> "mk_bool"
                    _ -> "mk_int"
    in "cons(" ++ makefun ++ "((" ++ showProx (typeRep x) ++ ")(" ++ showCExpression x m ++ ")), " ++ showCExpression l m ++ ")"
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

showCStmt :: Int -> Map.Map Int Int -> ClosureReturnEnv -> CStatement a -> String
showCStmt indent m _ (UpdateVar i x) = "\n" ++ indentStr indent ++ "v" ++ show i ++ " = " ++ showCExpression x m ++ ";"
showCStmt indent m closures (If cond t f) =
    "\n" ++ indentStr indent ++ "if " ++ showCExpression cond m ++ " {"
    ++  showCStmt (indent + 1) m closures t
    ++ "\n" ++ indentStr indent  ++ "} else {"
    ++ showCStmt (indent + 1) m closures f
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent m closures (While cond body) =
    "\n" ++ indentStr indent ++ "while " ++ showCExpression cond m ++ " {"
    ++ showCStmt (indent + 1) m closures body
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent m closures (BindExpr (x :: CExpression a) i y) =
    "\n" ++ indentStr indent ++ 
        case Map.lookup i closures of
            Just _ -> "Closure* v" ++ show i ++ " = " ++ showCExpression x m ++ ";"
            Nothing -> showProxVar ("v" ++ show i) (typeRep (Proxy :: Proxy a)) ++ " = " ++ showCExpression x m ++ ";"
    ++ showCStmt indent m closures y
showCStmt indent m closures (Seq x y) =
    showCStmt indent m closures x ++ showCStmt indent m closures y
showCStmt indent m closures (DefFun prox ifun params body) =
    "\n" ++ indentStr indent ++ showProxFunc ("v" ++ show ifun) params prox ++ " {"
    ++ showCStmt (indent + 1) m closures body
    ++ "\n" ++ indentStr indent ++ "}\n"
showCStmt indent m _ (DefVar i f) =  "\n" ++ indentStr indent ++ showProxVar ("v" ++ show i) (typeRep f) ++ " = " ++ showCExpression f m ++ ";"
showCStmt indent m _ (Return x) =  "\n" ++ indentStr indent ++ "return " ++ showCExpression x m ++ ";"
showCStmt indent _ _ (DefClosureStruct ifun p) =
    "\n" ++ indentStr indent ++ "typedef struct {\n"
    ++ showVars p
    ++ "} Env_v" ++ show ifun ++ ";\n"
    where
        showVars :: CParams -> String
        showVars [] = ""
        showVars [CParam ip tp] = "    " ++ showProxVar ("v" ++ show ip) (typeRep tp) ++ ";\n"
        showVars (first:rest) = showVars [first] ++ showVars rest
showCStmt indent _ _ (AllocClosure structId implId parentId directParams parentParams) =
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
            ++ " = v" ++ show ip ++ ";"
        ++ showDirect rest
    showDirect (_ : rest) = showDirect rest
    showParent [] = ""
    showParent (CParam ip _ : rest) =
        "\n" ++ indentStr indent ++ "env" ++ show structId ++ "->v" ++ show ip
            ++ " = ((Env_v" ++ show parentId ++ "*)env)->v" ++ show ip ++ ";"
        ++ showParent rest
    showParent (_ : rest) = showParent rest
showCStmt _ _ _ Skip = ""

findFirstReturn :: CStatement a -> CExpression a
findFirstReturn (Return x) = x
findFirstReturn (Seq x y) =
    case x of
        (Return i) -> i
        (DefFun {}) -> findFirstReturn y   -- ← skip into next, not into DefFun body
        _ -> findFirstReturn y             -- ← always continue into y
findFirstReturn (BindExpr _ _ y) = findFirstReturn y
findFirstReturn _ = error "no return"

removeFirstReturn :: CStatement a -> CStatement a
removeFirstReturn (Return _) = Skip
removeFirstReturn (Seq (Return _) y) = Seq Skip y
removeFirstReturn (Seq x@(DefFun {}) y) = Seq x (removeFirstReturn y)  -- ← don't descend into DefFun
removeFirstReturn (Seq x y) = Seq x (removeFirstReturn y)              -- ← keep x, strip from y
removeFirstReturn (BindExpr x i y) = BindExpr x i (removeFirstReturn y)
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

showParamMap :: [(Int, CParam)] -> String
showParamMap [] = ""
showParamMap [(i, p)] = "v" ++ show i ++ " (" ++ showCParams [p] ++ "), "
showParamMap (i:is) = showParamMap [i] ++ showParamMap is

showCType :: CType -> String
showCType CClosurePtr = "Closure*"
showCType (CTypeRep t) = showProx t
showCType CVoidPtr = "Void*"

showFunTypes :: [(Int, CType)] -> String
showFunTypes [] = ""
showFunTypes [(i,t)] = "v" ++ show i ++ " = " ++ showCType t ++ "\n"
showFunTypes (i:is) = showFunTypes [i] ++ showFunTypes is

-- Split top-level statement into (defFunsPart, mainBodyPart)
splitTopLevel :: CStatement a -> (CStatement a, CStatement a)
splitTopLevel (Seq l@(DefFun {}) y) =
    let (funs, body) = splitTopLevel y
    in (Seq l funs, body)
splitTopLevel (Seq l@(DefClosureStruct {}) y) =
    let (funs, body) = splitTopLevel y
    in (Seq l funs, body)
splitTopLevel (Seq Skip y) = splitTopLevel y
splitTopLevel (Seq x y) =
    let (funs, body) = splitTopLevel y
    in (funs, Seq x body)
splitTopLevel Skip = (Skip, Skip)
splitTopLevel x = (Skip, x)

-- MAIN

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
    let (cbody, closureEnv, liftenv, funs, defs) = lambdaLift c
    putStrLn $ showFunTypes (Map.toList funs)
    let imports = 
                "\n#include <stdbool.h>" ++
                "\n#include <stdio.h>" ++
                "\n#include <stdlib.h>" ++
                "\n#include <stdint.h>" ++
                "\n#include \"listLib.c\"\n"
    -- putStrLn $ showLiftEnv (Map.toList liftenv)
    
    let closureStructs = generateClosureStructs (Map.toList liftenv)
    let funDefs = makeFunDefs defs
    
    -- let cbody' = Seq closureStructs cbody

    -- let ret = findFirstReturn cbody'
    -- let bodyWithoutRet = removeFirstReturn cbody'
    -- let showBodyWithoutRet = showCStmt 0 Map.empty bodyWithoutRet
    -- let body = showBodyWithoutRet ++
    --         case trace (show (typeRep ret)) $ show (typeRep ret) of
    --             "Int" -> "\nint main(void) {\n" ++
    --                     "  printf(\"%d\\n\", " ++ showCExpression ret Map.empty ++ ");\n" ++
    --                     "  return 0;\n}\n"
    --             "[Int]" -> "\nint main(void) {\n" ++
    --                     "  printList(" ++ showCExpression ret Map.empty ++ ");\n" ++
    --                     "  return 0;\n}\n"
    --             _ -> "\nint main(void) {\n" ++
    --                     "  printf(\"%d\\n\", " ++ showCExpression ret Map.empty ++ ");\n" ++
    --                     "  return 0;\n}\n"
    -- let content = imports ++ "\n// Function Definitions" ++ funDefs ++ "\n\n// Compiled Program" ++ body
    
    let (funPart, mainBody) = splitTopLevel cbody
    let retExpr = findFirstReturn mainBody
    let mainBodyWithoutRet = removeFirstReturn mainBody
    
    let content = 
            "\n// imports" ++ imports ++
            "\n// function defitions" ++ funDefs ++
            "\n\n// closure defitions" ++ showCStmt 0 Map.empty Map.empty closureStructs ++
            "\n// function implementations" ++ showCStmt 0 Map.empty closureEnv funPart ++
            "\n// main" ++ 
                    case show (typeRep mainBody) of
                        "Int" -> "\nint main(void) {" 
                                    ++ showCStmt 1 Map.empty closureEnv mainBodyWithoutRet ++
                                "\n  printf(\"%d\\n\", " ++ showCExpression retExpr Map.empty ++ ");\n" ++
                                "  return 0;\n}\n"
                        "[Int]" -> "\nint main(void) {" ++ showCStmt 1 Map.empty closureEnv mainBodyWithoutRet ++
                                "\n  printList(" ++ showCExpression retExpr Map.empty ++ ");\n" ++
                                "  return 0;\n}\n"
                        _ -> error "cannot print"

    -- writing to file
    let fileName = "outputs/" ++ progName ++ "_output.c"
    handle <- openFile fileName WriteMode
    hPutStrLn handle content
    hClose handle
    putStrLn $ "Successfully wrote to " ++ fileName
