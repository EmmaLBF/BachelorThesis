{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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

