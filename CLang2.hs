{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{- HLINT ignore "Use first" -}

module CLang2 where
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map

import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import qualified NamedLang as NL
import Control.Monad.State

indentStr :: Int -> String
indentStr n = replicate (2 * n) ' '

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    UnitV :: CValue ()

data CExpression a where
    Var :: (Typeable a) => Int -> CExpression a
    LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
    LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
    Val :: CValue a -> CExpression a
    Not :: CExpression Bool -> CExpression Bool
    CallExpr :: CExpression (a -> b) -> CExpression a -> CExpression b
    Prod :: CExpression a -> CExpression b -> CExpression (a, b)  -- | Make a tuple
    Fst :: CExpression (a, b) -> CExpression a -- | Project left
    Snd :: CExpression (a, b) -> CExpression b -- | Project right

-- in theory statements shouldn't have a type? since they don't return anything?
data CStatement a where
  Return :: CExpression a -> CStatement a
  Bind :: (Typeable a) => CStatement a -> Int -> CStatement b -> CStatement b
  Seq :: CStatement a -> CStatement b -> CStatement b
  If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a
  DefFun :: (Typeable a) => Int -> CStatement a -> CStatement () -- needed to change this to ()
  DefVar :: (Typeable a) => Int -> CExpression a -> CStatement ()
  UpdateVar :: (Typeable a) => Int -> CExpression a -> CStatement () 
  While :: CExpression Bool -> CStatement a -> CStatement ()

fresh :: State Int Int
fresh = do
  n <- get
  modify (+1)
  return n

data Compiled a = Compiled
  { setup :: CStatement ()   -- | statements to run first (possibly empty)
  , result :: CExpression a  -- | the resulting expression
  }

type Trans a = State Int (Compiled a)

onlyExpr :: CExpression a -> Trans a
onlyExpr expr = return $ Compiled { setup = Return (Val UnitV), result = expr }

toStatement :: Compiled a -> CStatement a
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
  let stmt = Bind (If (result cc) (Return (result ct)) (Return (result cf))) n
           $ Return (Var n)
  return $ Compiled { setup = stmt, result = Var n }
translate (NL.Fix (NL.Lam i1 (NL.Lam i2 (NL.If cond base (NL.Apply (NL.Var i) arg))))) 
  | i1 == i = do
    ccond <- translate cond
    cbase <- translate base
    carg <- translate arg
    acc <- fresh
    n <- fresh
    let v_acc = Var acc :: CExpression a
        loop_body = Seq (UpdateVar i2 (result carg)) (UpdateVar acc (result cbase))
        wh = Seq (UpdateVar acc (result cbase)) (While (Not (result ccond)) loop_body)
        stmt = DefFun i2 (Seq (DefVar acc v_acc) wh)
    return $ Compiled { setup = stmt, result = Var n }
translate (NL.Fix f) = do
  cf <- translate f
  n <- fresh
  let stmt = DefFun n (toStatement cf)
  return $ Compiled { setup = stmt, result = Var n }
translate (NL.Lam i f) = do
  cf <- translate f
  let stmt = DefFun i (toStatement cf)
  return $ Compiled { setup = stmt, result = Var i }
translate (NL.Apply f x) = do
  cf <- translate f
  cx <- translate x
  onlyExpr (CallExpr (result cf) (result cx))

valueToLiteral :: CValue a -> a
valueToLiteral (IntV i) = i
valueToLiteral (BoolV i) = i
valueToLiteral UnitV = ()

evalExpr :: CExpression a -> Map Int Dynamic -> a
evalExpr (Val x) m = valueToLiteral x
evalExpr (Var i) m = case Map.lookup i m of
                      Just dyn -> case fromDynamic dyn of
                        Just v -> v
                        Nothing -> error "Type mismatch in env"
                      Nothing -> error "Variable not found"
evalExpr (LIntOp op lhs rhs) m =
  let lhs' = evalExpr lhs m
      rhs' = evalExpr rhs m
  in AL.binop op lhs' rhs'
evalExpr (LCmpOp op lhs rhs) m = 
  let lhs' = evalExpr lhs m
      rhs' = evalExpr rhs m
  in AL.cmpop op lhs' rhs'
evalExpr (CallExpr f arg) m = 
  let fn = evalExpr f m
      arg' = evalExpr arg m
  in fn arg'
evalExpr (Prod l r) m = 
  let l' = evalExpr l m
      r' = evalExpr r m
  in (l',r')
evalExpr (Fst p) m = 
  let p' = evalExpr p m
  in fst p'
evalExpr (Snd p) m = 
  let p' = evalExpr p m
  in snd p'
evalExpr (Not x) m = 
  let x' = evalExpr x m
  in not x'

evalStmt :: CStatement a -> Map Int Dynamic -> (a, Map Int Dynamic)
evalStmt (Return x) m = (evalExpr x m, m)
evalStmt (Bind x i y) m =
  let (x', m') = evalStmt x m
      m'' = Map.insert i (toDyn x') m'
  in evalStmt y m''
evalStmt (Seq x y) m = 
  let (_, m') = evalStmt x m
  in evalStmt y m'
evalStmt (If cond t e) m = 
  let cond' = evalExpr cond m
  in if cond' then evalStmt t m else evalStmt e m
evalStmt (While cond body) m =
  let cond' = evalExpr cond m
  in 
    if cond'
    then
      let (_, m') = evalStmt body m
      in evalStmt (While cond body) m'
    else
      ((), m)
evalStmt (DefFun i body) m =
  let fn = \arg ->  let m' = Map.insert i (toDyn arg) m
                        (body', _) = evalStmt body m
                      in body'
      m' = Map.insert i (toDyn fn) m
  in ((), m')
evalStmt (DefVar i x) m =
  let m' = Map.insert i (toDyn x) m
  in ((), m')
evalStmt (UpdateVar i x) m =
  let m' = Map.insert i (toDyn x) m
  in ((), m')

eval :: Compiled a -> Map Int Dynamic -> a
eval x m = 
  let (_, m') = evalStmt (setup x) m
  in evalExpr (result x) m'

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
showCStmt indent (DefFun i f) m = 
    "\n" ++ indentStr indent ++ "function (v" ++ show i ++ ") {"
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
        cl = evalState (translate nl)  c'
        -- ev = eval cl Map.empty
    -- print (ev 5)
    putStrLn "--- Translating CL ---"
    putStrLn $ showCStmt 0 (setup cl) Map.empty