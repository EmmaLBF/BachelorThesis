{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Avoid lambda" #-}
{- HLINT ignore "Use first" -}

module C where

import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import qualified NamedLang as NL
import qualified CLang2 as CL

import Data.Dynamic
import Control.Monad.State
import Data.Typeable
import Debug.Trace

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    UnitV :: CValue ()
    FunV  :: (CValue a -> CValue b) -> CValue (a -> b)
    PairV :: CValue a -> CValue b -> CValue (a, b)

data CParam where
  CParam :: Typeable a => Proxy a -> Int -> CParam

data CExpression a where
    Var :: (Typeable a) => Int -> CExpression a
    LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
    LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
    Val :: CValue a -> CExpression a
    Not :: CExpression Bool -> CExpression Bool
    CallExpr :: (Typeable a, Typeable b) => CExpression (a -> b) -> CExpression a -> CExpression b
    Prod :: (Typeable a, Typeable b) => CExpression a -> CExpression b -> CExpression (a, b)
    Fst  :: (Typeable a, Typeable b) => CExpression (a, b) -> CExpression a
    Snd  :: (Typeable a, Typeable b) => CExpression (a, b) -> CExpression b

data CStatement where
  BindExpr :: Typeable a => CExpression a -> Int -> CStatement -> CStatement
  Seq :: CStatement -> CStatement -> CStatement
  If :: CExpression Bool -> CStatement -> CStatement -> CStatement
  DefFun    :: (Typeable a, Typeable b) 
            => Proxy b      -- return type
            -> Int          -- function id
            -> [CParam]     -- parameters with types
            -> CStatement   -- body setup
            -> CExpression a -- body result
            -> CStatement
  DefVar :: Typeable a => Int -> CExpression a -> CStatement
  UpdateVar :: Typeable a => Int -> CExpression a -> CStatement
  While :: CExpression Bool -> CStatement -> CStatement
  Skip :: CStatement

translate :: CL.CStatement -> CStatement
translate

--   evalStmt (DefFunMultiArg (_ :: Proxy a) ifun iparams bodySetup (bodyResult :: CExpression b)) m =
--   let fn = mkFn iparams m
--       m' = Extend ifun fn m
--   in trace ("hello = " ++ show (typeRep fn)) $ m'
--   where
--     mkFn :: [Int] -> Env -> CValue a
--     mkFn [] env =
--       let env' = evalStmt bodySetup env
--       in unsafeCoerce $ evalExpr bodyResult env'
--     mkFn (i:is) env = unsafeCoerce $ FunV $ \(arg :: CValue a) -> mkFn is (Extend i arg env)


-- lambdalift :: CStatement -> CStatement
-- lambdalift (DefFun prox ifun iparam inner@(DefFun {}) arg) =
--   case lambdalift inner of
--     DefFunMultiArg _ _ iparams body res -> DefFunMultiArg prox ifun (iparam:iparams) body res
--     other -> DefFunMultiArg prox ifun [iparam] other arg
-- lambdalift (Seq x y) = Seq (lambdalift x) (lambdalift y)
-- lambdalift x = x


-- showCStmt indent (DefFunMultiArg prox ifun iparams stup res) =
--     "\n" ++ indentStr indent ++ show (typeRep prox) ++ " function" ++ show ifun ++ " (" ++ showParams iparams ++ ") {"
--     ++ showCStmt (indent + 1) m stup
--     ++ "\n" ++ indentStr (indent + 1) ++ "return " ++ showCExpression res ++ ";"
--     ++ "\n" ++ indentStr indent ++ "}"
--     where
--       showParams :: [Int] -> String
--       showParams [] = ""
--       showParams [i] = 
--         case lookupEnvType i of
--           Nothing -> error $ "variable not found in print " ++ show i
--           Just v -> show v ++ " v" ++ show i
--       showParams (i:is) = showParams [i] ++ ", " ++ showParams is