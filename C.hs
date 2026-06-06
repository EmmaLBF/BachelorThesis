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
import Control.Monad.State
import Data.Maybe

data CParam where
  CParam :: Int -> CType -> CParam
  CParamEnv  :: Int -> CParam -- void* env parameter
  deriving (Show, Ord)

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
instance Show CArg where
    show (CArg _ x) = showCExpression x Map.empty

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
    Abs :: CExpression Int -> CExpression Int
    Var :: CType -> Int -> CExpression a
    LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
    LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
    LBoolOp :: AL.BoolOp -> CExpression Bool -> CExpression Bool -> CExpression Bool
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
    DefEnvStruct :: Int -> CParams -> CStatement a  -- same, but fields are concrete types
    AllocClosure :: Int -> CStatement a -- closureId
    AllocEnv :: Int -> Int -> CParams -> CParams -> CStatement a -- envId parentId directParams parentParams
instance Eq (CStatement a) where
    l == r = (showCStmt 0 Map.empty Map.empty l) == (showCStmt 0 Map.empty Map.empty r)


type LiftEnv = Map.Map Int CParams
type Lifted a = [CStatement a]

type Hoisted a = [CStatement a]
type FunTypes = Map.Map Int CType

fresh :: State Int Int
fresh = do
  n <- get
  modify (+1)
  return n

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
freeVarsStmt _ = (Map.empty, Map.empty)

freeVars :: CStatement a -> CParamMap
freeVars s =
    let (free, bound) = freeVarsStmt s
    in Map.difference free bound

--------- HOISTING HELPERS

paramId :: CParam -> Int
paramId (CParam i _)  = i
paramId (CParamEnv i) = i

paramsToList :: CParams -> [Int]
paramsToList [] = []
paramsToList (CParam i _ : rest) = i : paramsToList rest
paramsToList (CParamEnv{} : rest) = paramsToList rest

paramsToListEnv :: CParams -> [Int]
paramsToListEnv [] = []
paramsToListEnv (CParam i _ : rest) = i : paramsToList rest
paramsToListEnv (CParamEnv i : rest) = i : paramsToList rest

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

collectArgs :: CExpression a -> (CExpression a, [CArg])
collectArgs (CallExpr _ tx f x) =
    let (f', as) = collectArgs (unsafeCoerce f)
    in (f', as ++ [CArg tx x])
collectArgs e = (e, [])

collectArgsApply :: CExpression a -> (CExpression a, [CArg])
collectArgsApply (ApplyClosure tx f x) =
    let (f', as) = collectArgsApply (unsafeCoerce f)
    in (f', as ++ [CArg tx x])
collectArgsApply e = (e, [])

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
        Just funSet -> if i `elem` paramsToList (Set.toList funSet)
            then GetEnvField t currFun i else Var t i
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

-- if any params are passed closures when called then they need to become closures
-- give list of deffun params, length of whole params list, and list of call args
makeClosureParams :: [CParam] -> Int -> [[CArg]] -> ([CParam], Set.Set Int)
makeClosureParams [] _ _ = ([], Set.empty)
makeClosureParams (CParam i t : ps) len argLists =
    let (ps', s) = makeClosureParams ps len argLists
        pos = len - length ps - 1
    in  if any (`isClosureArg` pos) argLists
        then (CParam i CTClosure : ps', Set.insert i s)
        else (CParam i t : ps', s)
makeClosureParams (p:ps) len argLists =
    let (ps', s) = makeClosureParams ps len argLists
    in (p:ps', s)

-- checks if arg at position i is a closure
isClosureArg :: [CArg] -> Int -> Bool
isClosureArg args i
    | i < length args = case args !! i of
        CArg _ (Val (ClosureV _)) -> True
        CArg CTClosure _ -> True
        _ -> False
    | otherwise = False


isParentOf :: Int -> Int -> Map.Map Int Int -> Bool
isParentOf child parent parentMap =
    case Map.lookup child parentMap of
        Just i
            | i == parent -> True
            | otherwise -> isParentOf i parent parentMap
        Nothing -> False

-- functions which immediately return another function need to return closures instead
-- if the function just returns a var, lookup the var in the map
    -- if its parent parameters contain the current params, the current function is its parent
        -- so it needs to be a closure
-- returns map of funid to the fun it makes a closure for
-- returns set of ids of parameter vars that also become closures (can't be in the same set as the functions because they are closures not functions that return closures
    -- so they can;t have the call statement)
makeClosureFactories :: CStatement a -> CStatement a -> ParentParams -> Map.Map Int Int -> ClosureFuns -> (CStatement a, ClosureFuns, ClosureParams)
makeClosureFactories (DefFun tret ifun params body) stmt m parents closures =
    let globalInfo = getGlobalInfo stmt emptyGlobalInfo
        funCallArgs = Map.findWithDefault [] ifun (callArgs globalInfo)
        funRet = findReturn body
        retId = case funRet of
            Just (Var _ i) -> i
            Just (Val (ClosureV i)) -> i
            _ -> (-1)
        returnsClosure = case funRet of
            Just (Val (ClosureV _)) -> True
            _ -> False
        (params', closureParams) = makeClosureParams params (length params) funCallArgs
    in  if tret == CTClosure
        then (DefFun tret ifun params body, closures, closureParams)
        else -- for the looping so that it eventually terminates
            if retId >= 0 then
                case Map.lookup retId m of
                    Just funSet -> -- returns a lifted function (must be made a closure to capture env)
                        if isParentOf retId ifun parents
                        then -- curr fun is parent
                            let parentParams = Set.toList funSet \\ params
                                directParams = Set.toList (Set.intersection (Set.fromList params) funSet)
                                allocEnv = AllocEnv retId ifun directParams parentParams
                                allocCls = if not returnsClosure then AllocClosure retId else Skip -- if its retun is alsready a closure we don't need to reallocate it
                                newBody = Seq allocEnv (Seq allocCls (replaceReturnClosure body retId))
                            in (DefFun CTClosure ifun params' newBody, Map.insert ifun retId closures, closureParams)
                        else (DefFun tret ifun params' body, closures, closureParams)
                    _ -> (DefFun tret ifun params' body, closures, closureParams)
            else (DefFun tret ifun params' body, closures, closureParams)
makeClosureFactories (Seq x y) stmt m parents closures =
    let (x', c, p) = makeClosureFactories x stmt m parents closures
        (y', c', p') = makeClosureFactories y stmt m parents closures
    in (Seq x' y', Map.union c c', Set.union p p')
makeClosureFactories x _ _ _ closures = (x, closures, Set.empty)

-- add env parameters to call sites of hoisted functions
-- if we call a function that is in our lifted set we need to make an env to its call list
addEnvParameterExpr :: CExpression a -> ParentParams -> CExpression a
addEnvParameterExpr (CallExpr tf tx f x) m =
    let (f', args) = collectArgs (CallExpr tf tx f x)
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
                    in AllocEnv i ifun directParams parentParams'
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
applyClosuresExpr :: CExpression a -> CStatement b -> ClosureFuns -> ClosureParams -> MergedMap -> State Int (CStatement a, CExpression a)
applyClosuresExpr (Var t i) _ closureFuns _ _ =
    case Map.lookup i closureFuns of
        Just _ -> return (AllocClosure i, Val (ClosureV i))
        _ -> return (Skip, Var t i)
applyClosuresExpr (CallExpr tf tx f x) stmt closureFuns closureParams m =
    let (f', args) = collectArgs (CallExpr tf tx f x)
    in case f' of
        Var _ i ->
            case Map.lookup i closureFuns of
                Just innerFun -> do -- the called function returns a closure
                    let newType = fromMaybe CTVoidPtr (getFunType stmt (followClosureIFun innerFun closureFuns))
                    let numArgs = Map.findWithDefault 1 i m
                    let (currArgs, otherArgs) = splitAt numArgs args
                    (pre, currArgs') <- applyClosuresArgs currArgs stmt closureFuns closureParams m
                    (pre', otherArgs') <- applyClosuresArgs otherArgs stmt closureFuns closureParams m
                    let closVar = DefVar CTClosure i (rebuildCall tf f' currArgs')
                    (castStmt, castExpr) <- applyWithCast newType (Val (ClosureV i)) otherArgs'
                    return (unsafeCoerce (Seq pre (Seq pre' (Seq closVar castStmt))), castExpr)
                _ -> -- it does not return a closure
                    if i `elem` closureParams  -- check if it is a closure itself
                    then do
                        (pre, args') <- applyClosuresArgs args stmt closureFuns closureParams m
                        (pre', stmt') <- applyWithCast CTVoidPtr f' args'
                        return $ unsafeCoerce (Seq pre' pre, stmt')
                    else
                        do
                        (pre, f'') <- applyClosuresExpr f stmt closureFuns closureParams m
                        (pre', x') <- applyClosuresExpr x stmt closureFuns closureParams m
                        return (unsafeCoerce $ Seq pre (unsafeCoerce pre'), CallExpr tf tx f'' x')
        _ -> do
            (pre, f'') <- applyClosuresExpr f stmt closureFuns closureParams m
            (pre', x') <- applyClosuresExpr x stmt closureFuns closureParams m
            return (unsafeCoerce $ Seq pre (unsafeCoerce pre'), CallExpr tf tx f'' x')
applyClosuresExpr (Ternary t c x y) stmt closureFuns closureParams m = do
    (pre, c') <- applyClosuresExpr c stmt closureFuns closureParams m
    (pre', x') <- applyClosuresExpr x stmt closureFuns closureParams m
    (pre'', y') <- applyClosuresExpr y stmt closureFuns closureParams m
    return (unsafeCoerce $ Seq pre (unsafeCoerce $ Seq pre' pre''), Ternary t c' x' y')
applyClosuresExpr (LIntOp op x y) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    (pre', y') <- applyClosuresExpr y stmt closureFuns closureParams m
    return (Seq pre pre', LIntOp op x' y')
applyClosuresExpr (LCmpOp op x y) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    (pre', y') <- applyClosuresExpr y stmt closureFuns closureParams m
    return (unsafeCoerce Seq pre pre', LCmpOp op x' y')
applyClosuresExpr (LBoolOp op x y) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    (pre', y') <- applyClosuresExpr y stmt closureFuns closureParams m
    return (Seq pre pre', LBoolOp op x' y')
applyClosuresExpr (ConsList t x y) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    (pre', y') <- applyClosuresExpr y stmt closureFuns closureParams m
    return (unsafeCoerce $ Seq pre (unsafeCoerce pre'), ConsList t x' y')
applyClosuresExpr (Not x) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    return (pre, Not x')
applyClosuresExpr (Abs x) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    return (pre, Abs x')
applyClosuresExpr (Box t x) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    return (unsafeCoerce pre, Box t x')
applyClosuresExpr (Unbox t x) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    return (unsafeCoerce pre, Unbox t x')
applyClosuresExpr (CastExpr t x) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    return (unsafeCoerce pre, CastExpr t x')
applyClosuresExpr (TailList t x) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    return (pre, TailList t x')
applyClosuresExpr (HeadList t x) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    return (unsafeCoerce pre, HeadList t x')
applyClosuresExpr (IsEmpty t x) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    return (unsafeCoerce pre, IsEmpty t x')
applyClosuresExpr (Fst t1 t2 x) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    return (unsafeCoerce pre, Fst t1 t2 x')
applyClosuresExpr (Snd t1 t2 x) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    return (unsafeCoerce pre, Snd t1 t2 x')
applyClosuresExpr (IndexList t x y) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    (pre', y') <- applyClosuresExpr y stmt closureFuns closureParams m
    return (unsafeCoerce (Seq pre (unsafeCoerce pre')), IndexList t x' y')
applyClosuresExpr (Prod t x y) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    (pre', y') <- applyClosuresExpr y stmt closureFuns closureParams m
    return (unsafeCoerce (Seq pre (unsafeCoerce pre')), Prod t x' y')
applyClosuresExpr (ApplyClosure t x y) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    (pre', y') <- applyClosuresExpr y stmt closureFuns closureParams m
    return (unsafeCoerce (Seq pre (unsafeCoerce pre')), ApplyClosure t x' y')
applyClosuresExpr x _ _ _ _ = return (Skip, x)

applyClosuresArgs :: [CArg] -> CStatement a -> ClosureFuns -> ClosureParams -> MergedMap -> State Int (CStatement a, [CArg])
applyClosuresArgs [] _ _ _ _ = return (Skip, [])
applyClosuresArgs [CArg t x] stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns  closureParams m
    return (unsafeCoerce pre, [CArg t x'])
applyClosuresArgs (arg : rest) stmt closureFuns closureParams m = do
    (pre, arg') <- applyClosuresArgs [arg] stmt closureFuns closureParams m
    (pre', rest') <- applyClosuresArgs rest stmt closureFuns closureParams m
    return (Seq pre pre', arg' ++ rest')

applyClosures :: CStatement a -> CStatement a -> ClosureFuns -> ClosureParams -> MergedMap -> State Int (CStatement a)
applyClosures (DefFun tret ifun params body) stmt closureFuns closureParams m = do
    body' <- applyClosures body stmt closureFuns closureParams m
    return $ DefFun tret ifun params body'
applyClosures (Seq x y) stmt closureFuns closureParams m = do
    x' <- applyClosures x stmt closureFuns closureParams m
    y' <- applyClosures y stmt closureFuns closureParams m
    return $ Seq x' y'
applyClosures (If c x y) stmt closureFuns closureParams m = do
    (pre, c') <- applyClosuresExpr c stmt closureFuns closureParams m
    x' <- applyClosures x stmt closureFuns closureParams m
    y' <- applyClosures y stmt closureFuns closureParams m
    return $ unsafeCoerce $ Seq pre (unsafeCoerce $ If c' x' y')
applyClosures (While c x) stmt closureFuns closureParams m = do
    (pre, c') <- applyClosuresExpr c stmt closureFuns closureParams m
    x' <- applyClosures x stmt closureFuns closureParams m
    return $ unsafeCoerce $ Seq pre (unsafeCoerce $ While c' x')
applyClosures (BindExpr t c i x) stmt closureFuns closureParams m = do
    (pre, c') <- applyClosuresExpr c stmt closureFuns closureParams m
    x' <- applyClosures x stmt closureFuns closureParams m
    return $ unsafeCoerce $ Seq pre (unsafeCoerce $ BindExpr t c' i x')
applyClosures (Return x) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    return $ Seq pre (Return x')
applyClosures (DefVar t i x) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    return $ unsafeCoerce $ Seq pre (DefVar t i x')
applyClosures (UpdateVar t i x) stmt closureFuns closureParams m = do
    (pre, x') <- applyClosuresExpr x stmt closureFuns closureParams m
    return $ unsafeCoerce $ Seq pre (UpdateVar t i x')
applyClosures x _ _ _ _ = return x

-- maps a function id (that returns a closure) to the function said closure contains
type ClosureFuns = Map.Map Int Int
type ClosureParams = Set.Set Int -- set of params which are closures
-- maps a function id to the number of new parameters it has (for later printing calls/apply corretcly)
type MergedMap = Map.Map Int Int
-- maps a function id to the set of Cparams of all of the functions it is nested in
type ParentParams = Map.Map Int (Set.Set CParam)

-- keep looping make closure factories
-- after we made the first ones and applied closures some funs return closures now
-- so we need to handle them until nothing changes
applyClosuresPasses :: CStatement a -> ParentParams -> Map.Map Int Int -> MergedMap -> Int -> CStatement a
applyClosuresPasses body parentParamsMap parents mergedMap freshCounter =
    let (body', closureFuns, closureParams) = makeClosureFactories body body parentParamsMap parents Map.empty
    in  if Map.null closureFuns then body'
        else
            let body''' = evalState (applyClosures body' body' closureFuns closureParams mergedMap) freshCounter
            in applyClosuresPasses body''' parentParamsMap parents mergedMap freshCounter

stripWrap :: CExpression a -> CExpression a
stripWrap (Unbox _ r) = unsafeCoerce (stripWrap r)
stripWrap (CastExpr _ r) = unsafeCoerce (stripWrap r)
stripWrap (Box _ r) = unsafeCoerce (stripWrap r)
stripWrap r = r


-- ESCAPE ANALYSIS

data GlobalInfo = GlobalInfo
    { usedEnvs :: Set.Set Int   -- var ids that flow into heap
    , closureUses :: Map.Map Int Int -- id of closure -> number of times used
    , functionCallsGlobal :: Map.Map Int Int -- id of function called -> number of times called
    , aliases :: Map.Map Int CArg
    , callArgs :: Map.Map Int [[CArg]]
    } deriving (Show)

emptyGlobalInfo :: GlobalInfo
emptyGlobalInfo = GlobalInfo Set.empty Map.empty Map.empty Map.empty Map.empty

mergeGlobalInfo :: GlobalInfo -> GlobalInfo -> GlobalInfo
mergeGlobalInfo a b = GlobalInfo
    (Set.union (usedEnvs a) (usedEnvs b))
    (Map.unionWith (+) (closureUses a) (closureUses b))
    (Map.unionWith (+) (functionCallsGlobal a) (functionCallsGlobal b))
    (Map.union (aliases a) (aliases b))
    (Map.unionWith (++) (callArgs a) (callArgs b))

getGlobalInfo :: CStatement a -> GlobalInfo -> GlobalInfo
getGlobalInfo (AllocEnv _ i _ _) m = m { usedEnvs = Set.insert i (usedEnvs m) }
getGlobalInfo (Seq x y) m = getGlobalInfo y (getGlobalInfo x m)
getGlobalInfo (If c x y) m = getGlobalInfo y (getGlobalInfo x (getGlobalInfoExpr c m))
getGlobalInfo (While c x) m = getGlobalInfo x (getGlobalInfoExpr c m)
getGlobalInfo (DefFun _ _ params body) m =
    let m' = foldr (\p acc ->
                case p of
                    CParamEnv i -> m { usedEnvs = Set.insert i (usedEnvs m) }
                    _ -> acc ) m params
    in getGlobalInfo body m'
getGlobalInfo (BindExpr _ x _ y) m = getGlobalInfo y (getGlobalInfoExpr x m)
getGlobalInfo (Return x) m = getGlobalInfoExpr x m
getGlobalInfo (DefVar t i x) m =
    let m' = case x of
                Val (EnvV j) -> m { aliases = Map.insert i (CArg t (Val (EnvV j))) (aliases m)}
                HeadList t2 var@Var{} -> m { aliases = Map.insert i (CArg t (HeadList t2 var)) (aliases m)}
                TailList t2 var@Var{} -> m { aliases = Map.insert i (CArg t (TailList t2 var)) (aliases m)}
                expr@Var{} -> m { aliases = Map.insert i (CArg t expr) (aliases m)}
                expr@Fst{} -> m { aliases = Map.insert i (CArg t expr) (aliases m)}
                expr@Snd{} -> m { aliases = Map.insert i (CArg t expr) (aliases m)}
                _ -> m
    in getGlobalInfoExpr x m'
getGlobalInfo (UpdateVar _ _ x) m = getGlobalInfoExpr x m
getGlobalInfo _ m = m

getGlobalInfoExpr :: CExpression a -> GlobalInfo -> GlobalInfo
getGlobalInfoExpr (Val (EnvV i)) m = m { usedEnvs = Set.insert i (usedEnvs m) }
getGlobalInfoExpr (Val (ClosureV i)) m = m { closureUses = Map.insertWith (+) i 1 (closureUses m) }
getGlobalInfoExpr (GetEnvField _ i _) m = m { usedEnvs = Set.insert i (usedEnvs m) }
getGlobalInfoExpr (CallExpr tf tx f x) m =
    let (func, args) = collectArgs (CallExpr tf tx f x)
        m' = case func of
                Var _ i -> m { functionCallsGlobal = Map.insertWith (+) i 1 (functionCallsGlobal m),
                                callArgs = Map.insertWith (++) i [args] (callArgs m)}
                _ -> getGlobalInfoExpr f m
    in foldr (\(CArg _ a) acc -> getGlobalInfoExpr a acc) m' args
getGlobalInfoExpr (ApplyClosure _ f x) m = getGlobalInfoExpr x (getGlobalInfoExpr f m)
getGlobalInfoExpr (Ternary _ c t e) m = getGlobalInfoExpr e (getGlobalInfoExpr t (getGlobalInfoExpr c m))
getGlobalInfoExpr (LIntOp _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr (LCmpOp _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr (LBoolOp _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr (ConsList _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr (Prod _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr (Fst _ _ x) m = getGlobalInfoExpr x m
getGlobalInfoExpr (Snd _ _ x) m = getGlobalInfoExpr x m
getGlobalInfoExpr (Not x) m = getGlobalInfoExpr x m
getGlobalInfoExpr (Abs x) m = getGlobalInfoExpr x m
getGlobalInfoExpr (IsEmpty _ x) m = getGlobalInfoExpr x m
getGlobalInfoExpr (HeadList _ x) m = getGlobalInfoExpr x m
getGlobalInfoExpr (TailList _ x) m = getGlobalInfoExpr x m
getGlobalInfoExpr (Box _ x) m = getGlobalInfoExpr x m
getGlobalInfoExpr (Unbox _ x) m = getGlobalInfoExpr x m
getGlobalInfoExpr (CastExpr _ x) m = getGlobalInfoExpr x m
getGlobalInfoExpr (IndexList _ x y) m = getGlobalInfoExpr y (getGlobalInfoExpr x m)
getGlobalInfoExpr _ m = m

-- GET FUNCTION INFO
data FunctionInfo = FunctionInfo
    {
      funId :: Int
    , funParams :: CParams
    , escapedVars :: Set.Set Int   -- var ids that flow into heap
    , varUses :: Map.Map Int Int
    , varDefs :: Map.Map Int CArg
    , escapedEnvs :: Set.Set Int   -- env ids that outlive the frame
    , allocedEnvs :: Set.Set Int
    , envUses :: Set.Set Int
    , functionCalls :: Map.Map Int Int
    , escapedClos :: Set.Set Int
    } deriving (Show)

emptyFunctionInfo :: FunctionInfo
emptyFunctionInfo =
    FunctionInfo 0 [] Set.empty Map.empty Map.empty Set.empty Set.empty Set.empty Map.empty Set.empty

mergeFunctionInfo :: FunctionInfo -> FunctionInfo -> FunctionInfo
mergeFunctionInfo a b = FunctionInfo
    (funId a)
    (funParams a)
    (Set.union (escapedVars a) (escapedVars b))
    (Map.unionWith max (varUses a) (varUses b))
    (Map.union (varDefs a) (varDefs b))
    (Set.union (escapedEnvs a) (escapedEnvs b))
    (Set.union (allocedEnvs a) (allocedEnvs b))
    (Set.union (envUses a) (envUses b))
    (Map.unionWith (+) (functionCalls a) (functionCalls b))
    (Set.union (escapedClos a) (escapedClos b))

-- (vars, envs)
getFunctionInfoExpr :: Bool -> CExpression a -> FunctionInfo -> FunctionInfo
getFunctionInfoExpr _ (GetEnvField _ envId _) r =
    r{ envUses = Set.insert envId (envUses r) }
getFunctionInfoExpr escapes (Var _ i) r =
    if escapes
    then r { escapedVars = Set.insert i (escapedVars r), varUses = Map.insertWith (+) i 1 (varUses r) }
    else r { varUses = Map.insertWith (+) i 1 (varUses r) }
getFunctionInfoExpr escapes (Val (EnvV i)) r =
    if escapes
    then r { escapedEnvs = Set.insert i (escapedEnvs r), envUses = Set.insert i (envUses r)  }
    else r
getFunctionInfoExpr escapes (Val (ClosureV i)) r =
    if escapes
    then r { escapedClos = Set.insert i (escapedClos r), varUses = Map.insertWith (+) i 1 (varUses r) }
    else r { varUses = Map.insertWith (+) i 1 (varUses r) }
getFunctionInfoExpr escapes (Not x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (Abs x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (Fst _ _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (Snd _ _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (IsEmpty _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (HeadList _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (TailList _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (CastExpr _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (Box _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (Unbox _ x) m = getFunctionInfoExpr escapes x m
getFunctionInfoExpr escapes (Ternary _ _ t e) m = getFunctionInfoExpr escapes t (getFunctionInfoExpr escapes e m)
getFunctionInfoExpr escapes (ConsList _ x y) m = getFunctionInfoExpr escapes x (getFunctionInfoExpr escapes y m)
getFunctionInfoExpr escapes (Prod _ x y) m = getFunctionInfoExpr escapes x (getFunctionInfoExpr escapes y m)
getFunctionInfoExpr escapes (LIntOp _ x y) m = getFunctionInfoExpr escapes  x (getFunctionInfoExpr escapes y m)
getFunctionInfoExpr escapes (LCmpOp _ x y) m = getFunctionInfoExpr escapes x (getFunctionInfoExpr escapes y m)
getFunctionInfoExpr escapes (LBoolOp _ x y) m = getFunctionInfoExpr escapes x (getFunctionInfoExpr escapes y m)
getFunctionInfoExpr escapes (CallExpr tf tx f x) m =
    let (func, args) = collectArgs (CallExpr tf tx f x)
        m' = case func of
                Var _ i -> m { functionCalls = Map.insertWith (+) i 1 (functionCalls m) }
                _ -> getFunctionInfoExpr escapes f m
    in foldr (\(CArg _ a) acc -> processArg a acc) m' args
    where
        processArg (Val (EnvV _)) m' = m'
        processArg arg m' = getFunctionInfoExpr True arg m'
getFunctionInfoExpr escapes (ApplyClosure _ f x) m = getFunctionInfoExpr escapes f (getFunctionInfoExpr escapes x m)
getFunctionInfoExpr escapes (IndexList _ x y) m = getFunctionInfoExpr escapes x (getFunctionInfoExpr escapes y m)
getFunctionInfoExpr _ _ m = m

getFunctionInfo :: CStatement a -> FunctionInfo -> FunctionInfo
getFunctionInfo (DefFun _ ifun params body) r =
    let r' = getFunctionInfo body (r { funId = ifun, funParams = params})
    in r' { funId = ifun, funParams = params}
getFunctionInfo (Seq x y) r = getFunctionInfo y (getFunctionInfo x r)
getFunctionInfo (Return x) r = getFunctionInfoExpr True x r
getFunctionInfo (BindExpr t x i y) r = getFunctionInfo y (getFunctionInfoExpr False x (r { varUses = Map.insertWith (+) i 1 (varUses r), varDefs = Map.insert i (CArg t x) (varDefs r) }))  -- x doesn't escape by being bound
getFunctionInfo (If c t e) r =
    let r' = getFunctionInfoExpr False c r
    in mergeFunctionInfo (getFunctionInfo t r') (getFunctionInfo e r')
getFunctionInfo (UpdateVar t i x) r = getFunctionInfoExpr False x (r { varUses = Map.insertWith (+) i 1 (varUses r), varDefs = Map.insert i (CArg t x) (varDefs r)})
getFunctionInfo (DefVar t i x) r = getFunctionInfoExpr False x (r { varUses = Map.insert i 0 (varUses r), varDefs = Map.insert i (CArg t x) (varDefs r) })
getFunctionInfo (While c x) r = getFunctionInfo x (getFunctionInfoExpr False c r)
getFunctionInfo (AllocEnv i parentId _ parentPs) r =
    r { allocedEnvs = Set.insert i (allocedEnvs r), envUses = if null parentPs then envUses r else Set.insert parentId (envUses r) }
getFunctionInfo (AllocClosure i) r =
    r { envUses = Set.insert i (envUses r) }
getFunctionInfo _ r = r



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

addPairType :: CType -> Set.Set (CType, CType) -> Set.Set (CType, CType)
addPairType t s =
    case t of
        CTPtr (CTPair tx ty) -> Set.insert (tx, ty) s
        CTPair tx ty -> Set.insert (tx, ty) s
        _ -> s

-- MAKE PAIR DEFS
collectPairTypes :: CStatement a -> State (Set.Set (CType, CType)) ()
collectPairTypes (DefFun t _ _ x) = modify (addPairType t) >> collectPairTypes x
collectPairTypes (Seq x y) = collectPairTypes x >> collectPairTypes y
collectPairTypes (If c x y) = collectPairTypes x >> collectPairTypes y >> collectPairTypesExpr c
collectPairTypes (While c x) = collectPairTypes x >> collectPairTypesExpr c
collectPairTypes (Return x) = collectPairTypesExpr x
collectPairTypes (DefVar t _ x) = modify (addPairType t) >> collectPairTypesExpr x
collectPairTypes (UpdateVar t _ x) = modify (addPairType t) >> collectPairTypesExpr x
collectPairTypes (BindExpr t x _ y) = modify (addPairType t) >> collectPairTypesExpr x >> collectPairTypes y
collectPairTypes _ = return ()

collectPairTypesExpr :: CExpression a -> State (Set.Set (CType, CType)) ()
collectPairTypesExpr (Prod t x y) = modify (addPairType t) >> collectPairTypesExpr x >> collectPairTypesExpr y
collectPairTypesExpr (Not x) = collectPairTypesExpr x
collectPairTypesExpr (Abs x) = collectPairTypesExpr x
collectPairTypesExpr (HeadList t x) = modify (addPairType t) >> collectPairTypesExpr x
collectPairTypesExpr (TailList t x) = modify (addPairType t) >> collectPairTypesExpr x
collectPairTypesExpr (IsEmpty t x) = modify (addPairType t) >> collectPairTypesExpr x
collectPairTypesExpr (IndexList t x y) = modify (addPairType t) >> collectPairTypesExpr x >> collectPairTypesExpr y
collectPairTypesExpr (Fst t _ x) = modify (addPairType t) >> collectPairTypesExpr x
collectPairTypesExpr (Snd t _ x) = modify (addPairType t) >> collectPairTypesExpr x
collectPairTypesExpr (Box t x) = modify (addPairType t) >> collectPairTypesExpr x
collectPairTypesExpr (Unbox t x) = modify (addPairType t) >> collectPairTypesExpr x
collectPairTypesExpr (CastExpr t x) = modify (addPairType t) >> collectPairTypesExpr x
collectPairTypesExpr (ConsList t x y) = modify (addPairType t) >> collectPairTypesExpr x >> collectPairTypesExpr y
collectPairTypesExpr (LIntOp _ x y) = collectPairTypesExpr x >> collectPairTypesExpr y
collectPairTypesExpr (LBoolOp _ x y) = collectPairTypesExpr x >> collectPairTypesExpr y
collectPairTypesExpr (LCmpOp _ x y) = collectPairTypesExpr x >> collectPairTypesExpr y
collectPairTypesExpr (CallExpr tx ty x y) = modify (addPairType tx) >> modify (addPairType ty) >> collectPairTypesExpr x >> collectPairTypesExpr y
collectPairTypesExpr (Ternary t x y z) = modify (addPairType t) >>  collectPairTypesExpr x >> collectPairTypesExpr y >> collectPairTypesExpr z
collectPairTypesExpr _ = return ()

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
-- printType (CTFun a b) = printType b ++ " (*)(" ++ printType a ++ ")"
printType (CTFun a b) = printFunPtr "" a b

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
showCValue (BoolV b) = if b then "true" else "false"
showCValue UnitV = "NULL"
showCValue (PairV x y) =
    "{ .fst = " ++ showCValue x ++ ", .snd = " ++ showCValue y ++ "}"
showCValue (FunV _) = "NULL"
showCValue (ListV l) =
  case l of
    [] -> ""
    (h:t) -> showCValue h ++ ", " ++ showCValue (ListV t)
showCValue (ClosureV i) = "c" ++ show i
showCValue (EnvV i) = "env" ++ show i

showListLibFunType :: CType -> String
showListLibFunType CTInt = "Int"
showListLibFunType CTBool = "Bool"
showListLibFunType _ = ""

showCExpression :: CExpression a -> MergedMap -> String
showCExpression (EmptyList _) _ = "NULL"
showCExpression (Val v) _ = showCValue v
showCExpression (Var _ i) _ = "v" ++ show i
showCExpression (Abs x) m = "abs(" ++ showCExpression x m ++ ")"
showCExpression (Not x) m = "!(" ++ showCExpression x m ++ ")"
showCExpression (LIntOp op x y) m = "(" ++ showCExpression x m ++ " " ++ CL.showBinOp op ++ " " ++ showCExpression y m ++ ")"
showCExpression (LBoolOp op x y) m = "(" ++ showCExpression x m ++ " " ++ CL.showBoolOp op ++ " " ++ showCExpression y m ++ ")"
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
            "(" ++ expr ++ ")->fn(" ++
            intercalate ", " (("(" ++ expr ++ ")->env") : map show argList) ++ ")"
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
        formatArgs args' = map show args'
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

showCStmt :: Int -> MergedMap -> Map.Map Int String -> CStatement a -> String
showCStmt indent m _ (UpdateVar _ i x) = "\n" ++ indentStr indent ++ "v" ++ show i ++ " = " ++ showCExpression x m ++ ";"
showCStmt indent m funs (If cond t f) =
    "\n" ++ indentStr indent ++ "if (" ++ showCExpression cond m ++ ") {"
    ++  showCStmt (indent + 1) m funs t
    ++ "\n" ++ indentStr indent  ++ "} else {"
    ++ showCStmt (indent + 1) m funs f
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent m funs (While cond body) =
    "\n" ++ indentStr indent ++ "while " ++ showCExpression cond m ++ " {"
    ++ showCStmt (indent + 1) m funs body
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent m funs (BindExpr ct x i y) =
    "\n" ++ indentStr indent ++ printDecl ("v" ++ show i) ct
    ++ " = " ++ showCExpression x m ++ ";"
    ++ showCStmt indent m funs y
showCStmt indent m funs (Seq x y) = showCStmt indent m funs x ++ showCStmt indent m funs y
showCStmt indent m funs (DefFun ct ifun params body) =
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
    ++ showCStmt (indent + 1) m funs body
    ++ "\n" ++ indentStr indent ++ "}\n"
showCStmt indent m _ (DefVar ct i x) =
    "\n" ++ indentStr indent ++
        case x of
            (Val (ClosureV i')) -> printDecl ("c" ++ show i') ct
            (Val (EnvV _)) -> printDecl ("env" ++ show i) ct
            _ ->
                case ct of
                    CTClosure -> printDecl ("c" ++ show i) ct
                    _ -> printDecl ("v" ++ show i) ct
    ++ " = " ++ showCExpression x m ++ ";"
showCStmt indent m _ (Return x) =  "\n" ++ indentStr indent ++ "return " ++ showCExpression x m ++ ";"
showCStmt indent _ _ (DefEnvStruct ifun p) =
    "\n" ++ indentStr indent ++ "typedef struct {\n"
    ++ concatMap (\case CParam ip tp -> "    " ++ printDecl ("v" ++ show ip) tp ++ ";\n"; _ -> "") p
    ++ "} Env_v" ++ show ifun ++ ";\n"
showCStmt indent _ _ (AllocClosure funId) =
    "\n" ++ indentStr indent ++ "Closure* c" ++ show funId ++ " = malloc(sizeof(Closure));"
    ++ "\n" ++ indentStr indent ++ "c" ++ show funId ++ "->env = env" ++ show funId ++ ";"
    ++ "\n" ++ indentStr indent ++ "c" ++ show funId
    ++ "->fn = (void* (*)(void*, void*))v" ++ show funId ++ ";"
showCStmt indent _ _ (AllocEnv envId parentId directParams parentParams) =
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
showCStmt _ _ _ Skip = ""

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

showListStmt :: [CStatement a] -> String
showListStmt = concatMap (showCStmt 0 Map.empty Map.empty)

printIntArgMap :: Map.Map Int CArg -> String
printIntArgMap m = intercalate ", " $ map (\(i, arg) -> "v" ++ show i ++ " -> " ++ show arg) (Map.toList m)

-- MAIN

generateEnvStructs :: Int -> Map.Map Int (Set.Set CParam) -> CStatement a
generateEnvStructs ifun liftenv =
    let envParams = maybe [] Set.toList (Map.lookup ifun liftenv)
    in DefEnvStruct ifun envParams

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

getStrFunTypes :: [CStatement a] -> Map.Map Int String -> Map.Map Int String
getStrFunTypes [] m = m
getStrFunTypes ((DefFun tret ifun _ _) : rest) m =
    let m' = Map.insert ifun (printType tret) m
    in getStrFunTypes rest m'
getStrFunTypes (_ : rest) m = getStrFunTypes rest m

-- collects a list of defs from the whole ast
getDefs :: CStatement a -> [CStatement a]
getDefs stmt@DefFun{} = [stmt]
getDefs (Seq x y) = getDefs x ++ getDefs y
getDefs _ = []

getClosureDefs :: CStatement a -> [Int]
getClosureDefs (DefFun tret ifun _ _) =
    case tret of
        CTClosure -> [ifun]
        _ -> []
getClosureDefs (Seq x y) = getClosureDefs x ++ getClosureDefs y
getClosureDefs _ = []

getFunTypes :: CStatement a -> Map.Map Int CType
getFunTypes (DefFun tret ifun _ _) = Map.insert ifun tret Map.empty
getFunTypes (Seq x y) = Map.union (getFunTypes x) (getFunTypes y)
getFunTypes _ = Map.empty

getFunType :: CStatement a -> Int -> Maybe CType
getFunType (DefFun tret ifun _ _) i | ifun == i = Just tret
getFunType (Seq x y) i =
    case getFunType x i of
        Nothing -> getFunType y i
        Just t -> Just t
getFunType _ _ = Nothing


{-
gcc ./outputs/mergeSortCall_output.c -o ./outputs/mergeSortCall_output
./outputs/mergeSortCall_output
-}

runLiftAndMerge :: Bool -> CStatement a -> Int -> (CStatement a, ParentParams, MergedMap)
runLiftAndMerge canMerge body freshInt =
    let ((body', (parentParamsMap, parentMap)), mergedMap) =
            if canMerge then
                let (merged, mergedLams) = mergeLambdas body body Map.empty
                    mergedMap' =  Map.foldlWithKey' (\acc k _ ->
                                    if Map.member k mergedLams
                                        then Map.insertWith (+) k 1 acc  -- already has merged param, just add env
                                        else Map.insertWith (+) k 2 acc  -- needs both default param and env
                                    ) mergedLams parentParamsMap
                in (runState (lambdaLift merged) (Map.empty, Map.empty), mergedMap')
            else
                let mergedMap' = Map.foldlWithKey' (\acc k _ -> Map.insertWith (+) k 2 acc) Map.empty parentParamsMap
                in (runState (lambdaLift body) (Map.empty, Map.empty), mergedMap')
        body'' = addEnvParameter body' parentParamsMap
        body''' = applyClosuresPasses body'' parentParamsMap parentMap mergedMap freshInt
        body'''' = addEnvAllocs body''' body''' parentParamsMap
    in (body'''', parentParamsMap, mergedMap)

run :: Typeable a => String -> AL.Lang a -> Bool -> IO ()
run progName progCode canMerge = do
    let libName = "\n#include \"" ++ "../"  ++ "listLib.c\"\n"
    let progPath =
            if canMerge then "mergedLams/" ++ progName
            else "baselines/" ++ progName
    -- let libName = "\n#include \"listLib.c\"\n"
    -- let progPath = progName

    let (nl, fresh') = runState (NL.translate progCode) 0
        (clBase, fresh'') = runState (CL.translate nl) fresh'
        clOpt = CL.optimizeBindings clBase
        c = translate clOpt

    let (cbody', parentParamsMap, mergedMap) = runLiftAndMerge canMerge c fresh''

    let finalBody = addBoxing cbody'
    let finalDefs = getDefs finalBody
    let strFunTypes = getStrFunTypes finalDefs Map.empty

    putStrLn "\n--- Printing C ---"
    let imports =   "\n#include <stdbool.h>" ++
                    "\n#include <stdio.h>" ++
                    "\n#include <stdlib.h>" ++
                    "\n#include <stdint.h>" ++
                    libName

    let envStructs = foldr Seq Skip (map (`generateEnvStructs` parentParamsMap) (Set.toList (usedEnvs (getGlobalInfo finalBody emptyGlobalInfo))))
    let pairTypes = execState (collectPairTypes finalBody) Set.empty
    let funDefs = showFunDefs finalDefs

    let (funPart, mainBody) = splitTopLevel finalBody
    let retExpr = findFirstReturn mainBody
    let mainBodyWithoutRet = removeFirstReturn mainBody
    let retImpl = showCExpression retExpr mergedMap
    let mainBodyImpl = showCStmt 1 mergedMap strFunTypes mainBodyWithoutRet
    let funImpl = showCStmt 0 mergedMap strFunTypes funPart

    let content =
            "\n// imports" ++ imports ++
            "\n// pair type defitions" ++ concatMap genPairDeclaration (Set.toList pairTypes) ++
            "\n// function defitions" ++ funDefs ++
            "\n\n// env defitions" ++ showCStmt 0 Map.empty strFunTypes envStructs ++
            "\n// function implementations" ++ funImpl ++
            "\n// main\nint main(void) {" ++ mainBodyImpl ++
                    case show (typeRep finalBody) of
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
    let progsQueen = [("nQueensCall", AL.nQueensCall)]

    -- let canMerge = True
    mapM_ (\(name, prog) -> run name prog False) progsInt
    mapM_ (\(name, prog) -> run name prog False) progsList
    mapM_ (\(name, prog) -> run name prog False) progsQueen

    mapM_ (\(name, prog) -> run name prog True) progsInt
    mapM_ (\(name, prog) -> run name prog True) progsList
    mapM_ (\(name, prog) -> run name prog True) progsQueen
