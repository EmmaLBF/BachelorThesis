{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Utils where

import CDefs
import Control.Monad.State
import Unsafe.Coerce
import qualified Data.Map as Map

fresh :: State Int Int
fresh = do
  n <- get
  modify (+1)
  return n

-- TYPE HELPERS

isPair :: CType -> Bool
isPair (CTPair _ _) = True
isPair (CTPtr (CTPair _ _)) = True
isPair _ = False

getTypeExpr :: CExpression a -> CType
getTypeExpr (HeadList t _) = t
getTypeExpr (TailList t _) = t
getTypeExpr (Var t _) = t
getTypeExpr (Not _) = CTBool
getTypeExpr (Abs _) = CTInt
getTypeExpr LIntOp{} = CTInt
getTypeExpr LCmpOp{} = CTBool
getTypeExpr LBoolOp{} = CTBool
getTypeExpr (Ternary t _ _ _) = t
getTypeExpr (Prod t _ _) = t
getTypeExpr (Fst _ t _) = t
getTypeExpr (Snd _ t _) = t
getTypeExpr (EmptyList t) = t
getTypeExpr (ConsList t _ _) = t
getTypeExpr (IsEmpty _ _) = CTBool
getTypeExpr (IndexList t _ _) = t
getTypeExpr ApplyClosure{} = CTVoidPtr
getTypeExpr (GetEnvField t _ _) = t
getTypeExpr (CallExpr tf _ _ _) = tf
getTypeExpr (CastExpr t _) = t
getTypeExpr (Box t _) = t
getTypeExpr (Unbox t _) = t
getTypeExpr (Val _) = CTVoidPtr

stripWrap :: CExpression a -> CExpression a
stripWrap (Unbox _ r) = unsafeCoerce (stripWrap r)
stripWrap (CastExpr _ r) = unsafeCoerce (stripWrap r)
stripWrap (Box _ r) = unsafeCoerce (stripWrap r)
stripWrap r = r

-- get a default value to init the new var before the if statement
defaultVal :: CType -> CValue a
defaultVal CTInt  = unsafeCoerce (IntV 0)
defaultVal CTBool = unsafeCoerce (BoolV False)
defaultVal (CTPair l r) =
    let l' = defaultVal l
    in unsafeCoerce (PairV (unsafeCoerce l') (unsafeCoerce defaultVal r))
defaultVal _ = unsafeCoerce UnitV


-- FUNDEF HELPERS

-- returns the deffun of fun i from the list of defs
findFunDef :: Int -> [CStatement a] -> Maybe (CStatement a)
findFunDef _ [] = Nothing
findFunDef i (def@(DefFun _ ifun _ _) : rest) =
    if i == ifun then Just def
    else findFunDef i rest
findFunDef _ _ = error "not valid def"

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

getFunType :: CStatement a -> Int -> Maybe CType
getFunType (DefFun tret ifun _ _) i | ifun == i = Just tret
getFunType (Seq x y) i =
    case getFunType x i of
        Nothing -> getFunType y i
        Just t -> Just t
getFunType _ _ = Nothing

-- collects a map of each fun id with the ids of its params from the whole ast
getFunsWithParams :: CStatement a -> Map.Map Int [Int]
getFunsWithParams (DefFun _ ifun params _) =
    Map.insert ifun (paramsToListEnv params) Map.empty
getFunsWithParams (Seq x y) = Map.union (getFunsWithParams x) (getFunsWithParams y)
getFunsWithParams _ = Map.empty

-- returns true if a function body ends in an if
endsInIf :: CStatement a -> Bool
endsInIf If {} = True
endsInIf (Seq _ y) = endsInIf y
endsInIf (BindExpr _ _ _ y) = endsInIf y
endsInIf _ = False


-- PARAMS HELPERS

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

merge :: (CParamMap, CParamMap) -> (CParamMap, CParamMap) -> (CParamMap, CParamMap)
merge (xfree, xbound) (yfree, ybound) = (Map.union xfree yfree, Map.union xbound ybound)

getEnvParams :: CParams -> [Int]
getEnvParams [] = []
getEnvParams [CParamEnv i] = [i]
getEnvParams [_] = []
getEnvParams (i:is) = getEnvParams [i] ++ getEnvParams is

-- for easier traversal

data SomeExpr = forall a. Some (CExpression a)

mapChildrenExpr :: (forall b. CExpression b -> CExpression b) -> CExpression a -> CExpression a
mapChildrenExpr f (Not x) = Not (f x)
mapChildrenExpr f (Abs x) = Abs (f x)
mapChildrenExpr f (Fst tr tp x) = Fst tr tp (f x)
mapChildrenExpr f (Snd tr tp x) = Snd tr tp (f x)
mapChildrenExpr f (HeadList t x) = HeadList t (f x)
mapChildrenExpr f (TailList t x) = TailList t (f x)
mapChildrenExpr f (IsEmpty t x) = IsEmpty t (f x)
mapChildrenExpr f (Box t x) = Box t (f x)
mapChildrenExpr f (Unbox t x) = Unbox t (f x)
mapChildrenExpr f (CastExpr t x) = CastExpr t (f x)
mapChildrenExpr f (LIntOp op x y) = LIntOp op (f x) (f y)
mapChildrenExpr f (LCmpOp op x y) = LCmpOp op (f x) (f y)
mapChildrenExpr f (LBoolOp op x y) = LBoolOp op (f x) (f y)
mapChildrenExpr f (Prod t x y) = Prod t (f x) (f y)
mapChildrenExpr f (ConsList t x y) = ConsList t (f x) (f y)
mapChildrenExpr f (IndexList t x y) = IndexList t (f x) (f y)
mapChildrenExpr f (ApplyClosure t x y) = ApplyClosure t (f x) (f y)
mapChildrenExpr f (CallExpr tf tx x y) = CallExpr tf tx (f x) (f y)
mapChildrenExpr f (Ternary t c x y) = Ternary t (f c) (f x) (f y)
mapChildrenExpr _ x = x

childrenExpr :: CExpression a -> [SomeExpr]
childrenExpr (Not x) = [Some x]
childrenExpr (Abs x) = [Some x]
childrenExpr (Box _ x) = [Some x]
childrenExpr (Unbox _ x) = [Some x]
childrenExpr (Fst _ _ x) = [Some x]
childrenExpr (Snd _ _ x) = [Some x]
childrenExpr (IsEmpty _ x) = [Some x]
childrenExpr (HeadList _ x) = [Some x]
childrenExpr (TailList _ x) = [Some x]
childrenExpr (CastExpr _ x) = [Some x]
childrenExpr (Prod _ x y) = [Some x, Some y]
childrenExpr (CallExpr _ _ x y) = [Some x, Some y]
childrenExpr (LIntOp _ x y) = [Some x, Some y]
childrenExpr (LCmpOp _ x y) = [Some x, Some y]
childrenExpr (LBoolOp _ x y) = [Some x, Some y]
childrenExpr (ConsList _ x y) = [Some x, Some y]
childrenExpr (IndexList _ x y) = [Some x, Some y]
childrenExpr (Ternary _ c t e) = [Some c, Some t, Some e]
childrenExpr _ = []

data SomeStmt = forall a. SomeStmt (CStatement a)

mapChildrenStmt :: (forall b. CStatement b -> CStatement b)
                -> (forall b. CExpression b -> CExpression b)
                -> CStatement a -> CStatement a
mapChildrenStmt fs _  (Seq x y)          = Seq (fs x) (fs y)
mapChildrenStmt fs fe (If c x y)         = If (fe c) (fs x) (fs y)
mapChildrenStmt fs fe (While c x)        = While (fe c) (fs x)
mapChildrenStmt fs _  (DefFun t i p b)   = DefFun t i p (fs b)
mapChildrenStmt fs fe (BindExpr t x i y) = BindExpr t (fe x) i (fs y)
mapChildrenStmt _  fe (DefVar t i x)     = DefVar t i (fe x)
mapChildrenStmt _  fe (UpdateVar t i x)  = UpdateVar t i (fe x)
mapChildrenStmt _  fe (Return x)         = Return (fe x)
mapChildrenStmt _  fe (AllocEnv e p d pp) =
    AllocEnv e p (Map.map (\(CArg t x) -> CArg t (fe x)) d)
                 (Map.map (\(CArg t x) -> CArg t (fe x)) pp)
mapChildrenStmt _  _  s = s

-- child statements only
childrenStmt :: CStatement a -> [SomeStmt]
childrenStmt (Seq x y)         = [SomeStmt x, SomeStmt y]
childrenStmt (If _ x y)        = [SomeStmt x, SomeStmt y]
childrenStmt (While _ x)       = [SomeStmt x]
childrenStmt (DefFun _ _ _ b)  = [SomeStmt b]
childrenStmt (BindExpr _ _ _ y) = [SomeStmt y]
childrenStmt _                 = []

-- child expressions held directly by a statement
childExprsStmt :: CStatement a -> [SomeExpr]
childExprsStmt (If c _ _)        = [Some c]
childExprsStmt (While c _)       = [Some c]
childExprsStmt (BindExpr _ x _ _) = [Some x]
childExprsStmt (DefVar _ _ x)    = [Some x]
childExprsStmt (UpdateVar _ _ x) = [Some x]
childExprsStmt (Return x)        = [Some x]
childExprsStmt (AllocEnv _ _ d pp) = [Some x | CArg _ x <- Map.elems d ++ Map.elems pp]
childExprsStmt _                 = []