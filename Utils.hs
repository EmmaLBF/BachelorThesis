{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import CDefs
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Unsafe.Coerce

-- ─────────────────────────────────────────────
--  Monad Helpers
-- ─────────────────────────────────────────────

fresh :: State Int Int
fresh = do
  n <- get
  modify (+ 1)
  return n

-- ─────────────────────────────────────────────
--  Type Helpers
-- ─────────────────────────────────────────────

isPair :: CType -> Bool
isPair (CTPair _ _) = True
isPair (CTPtr (CTPair _ _)) = True
isPair _ = False

getTypeExpr :: CExpression -> CType
getTypeExpr (HeadList t _) = t
getTypeExpr (TailList t _) = t
getTypeExpr (Var t _) = t
getTypeExpr (Not _) = CTBool
getTypeExpr (Abs _) = CTInt
getTypeExpr LIntOp {} = CTInt
getTypeExpr LCmpOp {} = CTBool
getTypeExpr LBoolOp {} = CTBool
getTypeExpr (Ternary t _ _ _) = t
getTypeExpr (Prod t _ _) = t
getTypeExpr (Fst _ t _) = t
getTypeExpr (Snd _ t _) = t
getTypeExpr (EmptyList t) = t
getTypeExpr (ConsList t _ _) = t
getTypeExpr (IsEmpty _ _) = CTBool
getTypeExpr (IndexList t _ _) = t
getTypeExpr ApplyClosure {} = CTPtr CTVoid
getTypeExpr (GetEnvField t _ _) = t
getTypeExpr (CallExpr tf _ _ _) = tf
getTypeExpr (CastExpr t _) = t
getTypeExpr (Box t _) = t
getTypeExpr (Unbox t _) = t
getTypeExpr (Val _) = CTPtr CTVoid

stripWrap :: CExpression -> CExpression
stripWrap (Unbox _ r) = stripWrap r
stripWrap (CastExpr _ r) = stripWrap r
stripWrap (Box _ r) = stripWrap r
stripWrap r = r

-- get a default value to init the new var before the if statement
defaultVal :: CType -> CValue a
defaultVal CTInt = unsafeCoerce (IntV 0)
defaultVal CTBool = unsafeCoerce (BoolV False)
defaultVal (CTPair l r) =
  let l' = defaultVal l
   in unsafeCoerce (PairV (unsafeCoerce l') (unsafeCoerce defaultVal r))
defaultVal _ = unsafeCoerce UnitV

-- ─────────────────────────────────────────────
--  DefFun Helpers
-- ─────────────────────────────────────────────

-- returns the deffun of fun i from the list of defs
findFunDef :: Int -> CStatement -> Maybe CStatement
findFunDef toFind stmt =
  let defs = getDefs stmt
   in findDef toFind defs
  where
    findDef _ [] = Nothing
    findDef i (def@(DefFun _ ifun _ _) : rest) =
      if i == ifun
        then Just def
        else findDef i rest
    findDef _ _ = error "not valid def"

-- collects a list of defs from the whole ast
getDefs :: CStatement -> [CStatement]
getDefs stmt@DefFun {} = [stmt]
getDefs (Seq x y) = getDefs x ++ getDefs y
getDefs _ = []

getDefIds :: CStatement -> [Int]
getDefIds (DefFun _ ifun _ _) = [ifun]
getDefIds (Seq x y) = getDefIds x ++ getDefIds y
getDefIds _ = []

getClosureDefs :: CStatement -> [Int]
getClosureDefs (DefFun tret ifun _ _) =
  case tret of
    CTClosure -> [ifun]
    _ -> []
getClosureDefs (Seq x y) = getClosureDefs x ++ getClosureDefs y
getClosureDefs _ = []

isCallToFun :: Int -> CExpression -> Bool
isCallToFun ifun expr@CallExpr {} =
  let (f', _) = collectArgs expr
   in case f' of Var _ i' | i' == ifun -> True; _ -> False
isCallToFun _ _ = False

getFunType :: CStatement -> Int -> Maybe CType
getFunType (DefFun tret ifun _ _) i | ifun == i = Just tret
getFunType (Seq x y) i =
  case getFunType x i of
    Nothing -> getFunType y i
    Just t -> Just t
getFunType _ _ = Nothing

-- collects a map of each fun id with the ids of its params from the whole ast
getFunsWithParams :: CStatement -> Map.Map Int [Int]
getFunsWithParams (DefFun _ ifun params _) =
  Map.insert ifun (paramsToList params) Map.empty
getFunsWithParams (Seq x y) = Map.union (getFunsWithParams x) (getFunsWithParams y)
getFunsWithParams _ = Map.empty

-- returns true if a function body ends in an if
endsInIf :: CStatement -> Bool
endsInIf If {} = True
endsInIf (Seq _ y) = endsInIf y
endsInIf _ = False

-- ─────────────────────────────────────────────
--  Params Helpers
-- ─────────────────────────────────────────────

paramId :: CParam -> Int
paramId (CParam i _) = i
paramId (CParamEnv i) = i

isElemParamSet :: Int -> Set.Set CParam -> Bool
isElemParamSet i s = i `elem` paramsToList (Set.toList s)

paramsToList :: CParams -> [Int]
paramsToList [] = []
paramsToList (CParam i _ : rest) = i : paramsToList rest
paramsToList (CParamEnv i : rest) = i : paramsToList rest

merge :: (CParamMap, CParamMap) -> (CParamMap, CParamMap) -> (CParamMap, CParamMap)
merge (xfree, xbound) (yfree, ybound) = (Map.union xfree yfree, Map.union xbound ybound)

getEnvParams :: CParams -> [Int]
getEnvParams [] = []
getEnvParams [CParamEnv i] = [i]
getEnvParams [_] = []
getEnvParams (i : is) = getEnvParams [i] ++ getEnvParams is

-- ─────────────────────────────────────────────
--  Traversal Helpers
-- ─────────────────────────────────────────────

mapChildrenExpr :: (CExpression -> CExpression) -> CExpression -> CExpression
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

childrenExpr :: CExpression -> [CExpression]
childrenExpr (Not x) = [x]
childrenExpr (Abs x) = [x]
childrenExpr (Box _ x) = [x]
childrenExpr (Unbox _ x) = [x]
childrenExpr (Fst _ _ x) = [x]
childrenExpr (Snd _ _ x) = [x]
childrenExpr (IsEmpty _ x) = [x]
childrenExpr (HeadList _ x) = [x]
childrenExpr (TailList _ x) = [x]
childrenExpr (CastExpr _ x) = [x]
childrenExpr (Prod _ x y) = [x, y]
childrenExpr (CallExpr _ _ x y) = [x, y]
childrenExpr (LIntOp _ x y) = [x, y]
childrenExpr (LCmpOp _ x y) = [x, y]
childrenExpr (LBoolOp _ x y) = [x, y]
childrenExpr (ConsList _ x y) = [x, y]
childrenExpr (IndexList _ x y) = [x, y]
childrenExpr (Ternary _ c t e) = [c, t, e]
childrenExpr _ = []

mapChildrenStmt :: (CStatement -> CStatement) -> (CExpression -> CExpression) -> CStatement -> CStatement
mapChildrenStmt fs _ (Seq x y) = Seq (fs x) (fs y)
mapChildrenStmt fs fe (If c x y) = If (fe c) (fs x) (fs y)
mapChildrenStmt fs fe (While c x) = While (fe c) (fs x)
mapChildrenStmt fs _ (DefFun t i p b) = DefFun t i p (fs b)
mapChildrenStmt _ fe (DefVar t i x) = DefVar t i (fe x)
mapChildrenStmt _ fe (UpdateVar t i x) = UpdateVar t i (fe x)
mapChildrenStmt _ fe (Return x) = Return (fe x)
mapChildrenStmt _ fe (AllocEnv e p params) =
  AllocEnv e p (Map.map (\(CArg t x) -> CArg t (fe x)) params)
mapChildrenStmt _ _ s = s

-- child statements only
childrenStmt :: CStatement -> [CStatement]
childrenStmt (Seq x y) = [x, y]
childrenStmt (If _ x y) = [x, y]
childrenStmt (While _ x) = [x]
childrenStmt (DefFun _ _ _ b) = [b]
childrenStmt _ = []

-- child expressions held directly by a statement
childExprsStmt :: CStatement -> [CExpression]
childExprsStmt (If c _ _) = [c]
childExprsStmt (While c _) = [c]
childExprsStmt (DefVar _ _ x) = [x]
childExprsStmt (UpdateVar _ _ x) = [x]
childExprsStmt (Return x) = [x]
childExprsStmt (AllocEnv _ _ params) = [x | CArg _ x <- Map.elems params]
childExprsStmt _ = []

mapChildrenStmtM :: Monad m => (CStatement -> m CStatement) -> (CExpression -> m CExpression) -> CStatement -> m CStatement
mapChildrenStmtM fs fe stmt = case stmt of
  Seq x y -> Seq <$> fs x <*> fs y
  If c x y -> If <$> fe c <*> fs x <*> fs y
  While c x -> While <$> fe c <*> fs x
  DefFun t i p b -> DefFun t i p <$> fs b
  DefVar t i x -> DefVar t i <$> fe x
  UpdateVar t i x -> UpdateVar t i <$> fe x
  Return x -> Return <$> fe x
  AllocEnv e p params -> AllocEnv e p <$> traverse (\(CArg t x) -> CArg t <$> fe x) params
  s -> return s