{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{- HLINT ignore "Use first" -}

module CLang2 where
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map

import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import qualified NamedLang as NL
import Control.Monad.State
import Unsafe.Coerce
import Data.Typeable
import Debug.Trace

indentStr :: Int -> String
indentStr n = replicate (2 * n) ' '

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    UnitV :: CValue ()
    FunV  :: (CValue a -> CValue b) -> CValue (a -> b)
    PairV :: CValue a -> CValue b -> CValue (a, b)

deriving instance Typeable a => Typeable (CValue a)

data CExpression a where
    Var :: (Typeable a) => Int -> CExpression a
    LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
    LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
    Val :: CValue a -> CExpression a
    Not :: CExpression Bool -> CExpression Bool
    CallExpr :: (Typeable a, Typeable b) => CExpression (a -> b) -> CExpression a -> CExpression b
    Prod :: CExpression a -> CExpression b -> CExpression (a, b)
    Fst :: CExpression (a, b) -> CExpression a
    Snd :: CExpression (a, b) -> CExpression b

-- in theory statements shouldn't have a type? since they don't return anything?
data CStatement a where
  Return :: Typeable a => CExpression a -> CStatement a
  Bind :: Typeable a => CStatement a -> Int -> CStatement b -> CStatement b
  Seq :: CStatement a -> CStatement b -> CStatement b
  If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a

  -- Do not return anything
  DefFun :: (Typeable a, Typeable b) => Proxy a -> Int -> Int -> CStatement b -> CStatement ()
  DefVar :: Typeable a => Int -> CExpression a -> CStatement ()
  UpdateVar :: Typeable a => Int -> CExpression a -> CStatement ()
  While :: CExpression Bool -> CStatement a -> CStatement ()

fresh :: State Int Int
fresh = do
  n <- get
  modify (+1)
  return n

data Compiled a = Compiled
  { setup :: CStatement ()
  , result :: CExpression a
  }

type Trans a = State Int (Compiled a)

onlyExpr :: CExpression a -> Trans a
onlyExpr expr = return $ Compiled { setup = Return (Val UnitV), result = expr }

toStatement :: Typeable a => Compiled a -> CStatement a
toStatement c = Seq (setup c) (Return (result c))

translate :: forall a. NL.NamedLang a -> State Int (Compiled a)
translate (NL.LInt n) = onlyExpr (Val (IntV n))
translate (NL.LBool b) = onlyExpr (Val (BoolV b))
translate (NL.Var n) = onlyExpr (Var n)
translate (NL.Prod a b) = do
  ca <- translate a
  cb <- translate b
  onlyExpr (Prod (result ca) (result cb))
translate (NL.Fst p) = do
  cp <- translate p
  onlyExpr (Fst (result cp))
translate (NL.Snd p) = do
  cp <- translate p
  onlyExpr (Snd (result cp))
translate (NL.LIntOp op l r) = do
  cl <- translate l
  cr <- translate r
  onlyExpr (LIntOp op (result cl) (result cr))
translate (NL.LCmpOp op l r) = do
  cl <- translate l
  cr <- translate r
  onlyExpr (LCmpOp op (result cl) (result cr))
translate (NL.If cond t f) = do
  cc <- translate cond
  ct <- translate t
  cf <- translate f
  n  <- fresh
  let branch = If (result cc) (Return (result ct)) (Return (result cf))
      ret = Return (Var n `asTypeOf` result ct) -- should work since both branches have the same type?
      stmt = Seq (Bind branch n ret) (Return (Val UnitV))
  return $ Compiled { setup = stmt, result = Var n }
-- translate (NL.Fix (NL.Lam i1 (NL.Lam i2 (NL.If cond base (NL.Apply (NL.Var i) arg))))) 
--   | i1 == i = do
--     ccond <- translate cond
--     cbase <- translate base
--     carg <- translate arg
--     acc <- fresh
--     n <- fresh
--     let v_acc = Var acc :: CExpression a
--         loop_body = Seq (UpdateVar i2 (result carg)) (UpdateVar acc (result cbase))
--         wh = Seq (UpdateVar acc (result cbase)) (While (Not (result ccond)) loop_body)
--         stmt = DefFun i2 (Seq (DefVar acc v_acc) wh)
--     return $ Compiled { setup = stmt, result = Var n }
-- translate (NL.Fix (NL.Lam iparam body)) = do
--   cf <- translate body
--   -- ifun <- fresh
--   -- iparam <- fresh
--   let stmt = DefFun iparam iparam (toStatement cf)
--   return $ Compiled { setup = stmt, result = Var iparam }
translate (NL.Fix f) = do
  cf <- translate f
  n <- fresh  -- this must not clash with any Lam parameter
  let v = Var n :: CExpression a
      call = CallExpr (result cf) (unsafeCoerce (result cf))
      bindStmt = Bind (Return call) n (Return v)
      stmt = Seq (Seq (setup cf) bindStmt) (Return (Val UnitV))
  return $ Compiled { setup = stmt, result = Var n }
-- translate (NL.Fix f) = do
--   cf <- translate f
--   fixId <- fresh
--   argId <- fresh
--   fixVarId <- fresh  -- NEW: variable to hold the fixed-point result
  
--   let selfVar = Var fixId :: CExpression (a -> a)
--       selfCall = CallExpr selfVar (Var argId) :: CExpression a
--       -- Replace the original function result with the self-call
--       recBody = cf { result = selfCall }
--       defStmt = DefFun (Proxy :: Proxy a) fixId argId (toStatement recBody)
--       bindStmt = Bind defStmt fixVarId (Return (Var fixVarId))
  
--   return $ Compiled { setup = bindStmt, result = Var fixVarId }
  -- case f of
  --   (NL.Lam i _) -> do
  --     cf <- translate f
  -- -- iparam <- fresh
  --     let v = Var i :: CExpression a
  --         stmt = Bind (setup cf) i (Return v)
  --     return $ Compiled { setup = stmt, result = Var i }
  --   _ -> do
  --     cf <- translate f
  --     n <- fresh
  --     -- iparam <- fresh
  --     let v = Var n :: CExpression a
  --         stmt = Bind (setup cf) n (Return v)
  --     return $ Compiled { setup = stmt, result = Var n }
-- translate (NL.Fix f) = do
--   cf <- translate f
--   n <- fresh
--   -- iparam <- fresh
--   let v = Var n :: CExpression a
--       stmt = Bind (setup cf) n (Return v)
--   return $ Compiled { setup = stmt, result = Var n }
translate (NL.Lam i (f :: NL.NamedLang b)) = do
  cf <- translate f
  ifun <- fresh
  let stmt = DefFun (Proxy :: Proxy a) ifun i (setup cf)
  return $ Compiled { setup = stmt, result = Var ifun }
translate (NL.Apply f x) = do
  cf <- translate f
  cx <- translate x
  onlyExpr (CallExpr (result cf) (result cx))

-- data EvalValue where
--   EvalValue :: Typeable a => CValue a -> EvalValue

-- valueToLiteral :: CValue a -> a
-- valueToLiteral (IntV i) = i
-- valueToLiteral (BoolV i) = i
-- valueToLiteral UnitV = ()

unInt :: CValue Int -> Int
unInt (IntV x) = x

unBool :: CValue Bool -> Bool
unBool (BoolV x) = x

data Env where
  Empty :: Env
  Extend :: Typeable a => Int -> CValue a -> Env -> Env

lookupEnv :: forall a. Typeable a => Int -> Env -> Maybe (CValue a)
lookupEnv _ Empty = Nothing
lookupEnv i1 (Extend i2 x remainder) 
  | i1 == i2 = trace ("lookup " ++ show i1 ++ 
                      ": stored=" ++ show (typeRep x) ++ 
                      " expect=" ++ show (typeRep (Proxy :: Proxy a))) $
               cast x
  | otherwise = lookupEnv i1 remainder

evalExpr :: CExpression a -> Env -> CValue a
evalExpr (Val x) _ = x
evalExpr (Var i) m =
  case lookupEnv i m of
    Just v -> v
    Nothing -> error ("Variable not found: " ++ show i)
evalExpr (LIntOp op lhs rhs) m =
  let lhs' = evalExpr lhs m
      rhs' = evalExpr rhs m
  in IntV (AL.binop op (unInt lhs') (unInt rhs'))
evalExpr (LCmpOp op lhs rhs) m =
  let lhs' = evalExpr lhs m
      rhs' = evalExpr rhs m
  in BoolV (AL.cmpop op (unInt lhs') (unInt rhs'))
evalExpr (CallExpr f arg) m =
  let FunV fn = evalExpr f m
      arg'    = evalExpr arg m
  in fn arg'
evalExpr (Prod l r) m =
  let l' = evalExpr l m
      r' = evalExpr r m
  in PairV l' r'
evalExpr (Fst p) m =
  let PairV x _ = evalExpr p m
  in x
evalExpr (Snd p) m =
  let PairV _ x = evalExpr p m
  in x
evalExpr (Not x) m =
  let x' = evalExpr x m
  in BoolV (not (unBool x'))

-- evalExprDyn :: CExpression a -> Map Int EvalValue -> Dynamic
-- evalExprDyn (CallExpr f arg) m =
--   let fn   = evalExprDyn f m
--       arg' = evalExprDyn arg m
--   in case fromDynamic fn of
--        Just (g :: Dynamic -> Dynamic) -> g arg'
--        Nothing -> error $
--          "CallExpr: expected function, got "
--          ++ show (dynTypeRep fn)
-- evalExprDyn f m = case Map.lookup i m of
--     Just dyn -> case fromDynamic dyn of
--       Just fn -> fn
--       Nothing -> error $ "Type mismatch in evalExprDyn | got " ++ show (dynTypeRep dyn)
--     Nothing -> error "Variable not found in evalExprDyn"
--   where
--     i = case f of
--           Var i -> i
--           _     -> error "evalExprDyn: not a Var"

-- type Eval a = State Int (EvalValue, Map Int EvalValue)

evalStmt :: CStatement a -> Env -> (CValue a, Env)
evalStmt (Return x) m = (evalExpr x m, m)
evalStmt (Bind x i y) m =
  let (x', m') = evalStmt x m
      m'' = Extend i x' m'
  in evalStmt y m''
evalStmt (Seq x y) m =
  let (_, m') = evalStmt x m
  in evalStmt y m'
evalStmt (If cond t e) m =
  let cond' = evalExpr cond m
  in if unBool cond' then evalStmt t m else evalStmt e m
evalStmt (While cond body) m =
  let cond' = evalExpr cond m
  in
    if unBool cond'
    then
      let (_, m') = evalStmt body m
      in evalStmt (While cond body) m'
    else
      (UnitV, m)
evalStmt (DefFun (_ :: Proxy a) ifun iparam (body :: CStatement b)) m =
  let fn :: CValue a -> CValue b
      fn arg =
        let m' = Extend iparam arg m
            (res, _) = evalStmt body m'
            debugRes = trace ("fn result: " ++ show (typeRep res)) res
        in debugRes  
      m'' = Extend ifun (FunV fn :: CValue (a -> b)) m
  in (UnitV, m'')
evalStmt (DefVar i x) m =
  let m' = Extend i (evalExpr x m) m
  in (UnitV, m')
evalStmt (UpdateVar i x) m =
  let m' = Extend i (evalExpr x m) m
  in ( UnitV, m')

eval :: Compiled (a->b) -> CValue a -> Env -> CValue b
eval x arg m =
  let (_, m') = evalStmt (setup x) m
      FunV fn = evalExpr (result x) m'
  in fn arg

showBinOp :: AL.BinOp -> String
showBinOp Plus  = "+"
showBinOp Min   = "-"
showBinOp Times = "*"
showBinOp Div   = "/"
showBinOp Mod   = "%"

showCmpOp :: AL.CmpOp -> String
showCmpOp Eq = "=="
showCmpOp Lt = "<"
showCmpOp Gt = ">"

showCValue :: CValue a -> String
showCValue (IntV n)  = show n
showCValue (BoolV b) = show b
showCValue UnitV = show ""

showCStmt :: Int -> CStatement a -> Map Int Dynamic -> String
showCStmt indent (UpdateVar i x) m = "\n" ++ indentStr indent ++ "v" ++ show i ++ " =~ " ++ showCExpression indent x m
showCStmt indent (If cond t f) m =
    "\n" ++ indentStr indent ++ "if " ++ showCExpression indent cond m ++ " {"
    ++  showCStmt (indent + 1) t m
    ++ "\n" ++ indentStr indent ++ "} else {"
    ++ showCStmt (indent + 1) f m
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent (While cond body) m =
    "\n" ++ indentStr indent ++ "while " ++ showCExpression indent cond m ++ " {"
    ++ showCStmt (indent + 1) body m
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent (Bind x i y) m =
    "\n" ++ indentStr indent ++ "bind v" ++ show i ++ " = {"
    ++ showCStmt (indent + 1) x m
    ++ "\n" ++ indentStr indent ++ "} in {"
    ++ showCStmt (indent + 1) y m
    ++ "\n" ++  indentStr indent ++ "}"
showCStmt indent (Seq x y) m =
    showCStmt indent x m ++ showCStmt indent y m
showCStmt indent (DefFun _ ifun iparam f) m =
    "\n" ++ indentStr indent ++ "function" ++ show ifun ++ " (v" ++ show iparam ++ ") {"
    ++ showCStmt (indent + 1) f m
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent (Return x) m =  "\n" ++ indentStr indent ++ "[Return " ++ showCExpression indent x m ++ "]"
showCStmt indent (DefVar i f) m =  "\n" ++ indentStr indent ++ "v" ++ show i ++ " = " ++ showCExpression indent f m

showCExpression :: Int -> CExpression a -> Map Int Dynamic -> String
showCExpression _ (Var i) _ = "v" ++ show i
showCExpression indent (Not x) m = "!" ++ showCExpression indent x m
showCExpression indent (LIntOp op x y) m = "(" ++ showCExpression indent x m ++ " " ++ showBinOp op ++ " " ++ showCExpression indent y m ++ ")"
showCExpression indent (LCmpOp op x y) m = "(" ++ showCExpression indent x m ++ " " ++ showCmpOp op ++ " " ++ showCExpression indent y m ++ ")"
showCExpression _ (Val v) _ = showCValue v
showCExpression indent (CallExpr f arg) m = showCExpression indent f m ++ "(" ++ showCExpression indent arg m ++ ")"
showCExpression indent (Prod l r) m = "(" ++ showCExpression indent l m ++ "," ++ showCExpression indent r m ++ ")"
showCExpression indent (Fst p) m = showCExpression indent p m ++ "[0]"
showCExpression indent (Snd p) m = showCExpression indent p m ++ "[1]"

main :: IO ()
main = do
    let (nl, c') = NL.translate 0 AL.fac
        cl = evalState (translate nl) c'
        ev = eval cl (IntV 5) Empty
    putStrLn "--- Translating CL ---"
    putStrLn $ showCStmt 0 (setup cl) Map.empty
    putStrLn $ showCValue ev