{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Avoid lambda" #-}
{- HLINT ignore "Use first" -}

module CLang2 where
import Data.Dynamic

import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import qualified NamedLang as NL
import Control.Monad.State
import Data.Typeable
import Debug.Trace

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    UnitV :: CValue ()
    FunV  :: (CValue a -> CValue b) -> CValue (a -> b)
    PairV :: CValue a -> CValue b -> CValue (a, b)

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
  DefFun :: (Typeable a, Typeable b) => Proxy a -> Int -> Int -> CStatement -> CExpression b -> CStatement
  DefVar :: Typeable a => Int -> CExpression a -> CStatement
  UpdateVar :: Typeable a => Int -> CExpression a -> CStatement
  While :: CExpression Bool -> CStatement -> CStatement
  Skip :: CStatement

fresh :: State Int Int
fresh = do
  n <- get
  modify (+1)
  return n

data Compiled a = Compiled
  { setup :: CStatement
  , result :: CExpression a
  }

onlyExpr :: CExpression a -> State Int (Compiled a)
onlyExpr expr = return $ Compiled { setup = Skip, result = expr }

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
translate (NL.Apply f x) = do
  cf <- translate f
  cx <- translate x
  onlyExpr (CallExpr (result cf) (result cx))
translate (NL.If cond t f) = do
  cc <- translate cond
  ct <- translate t
  cf <- translate f
  n  <- fresh
  let stmt = Seq (DefVar n (Val UnitV))
                   (If (result cc)
                       (Seq (setup ct) (UpdateVar n (result ct)))
                       (Seq (setup cf) (UpdateVar n (result cf))))
  return $ Compiled { setup = stmt, result = Var n }
translate (NL.Lam arg i (f :: NL.NamedLang b)) = do
  cf <- translate f
  ifun <- fresh
  let stmt = DefFun arg ifun i (setup cf) (result cf)
  return $ Compiled { setup = stmt, result = Var ifun }
translate (NL.Fix f) = do
  cf <- translate f
  n <- fresh
  let v = Var n :: CExpression a
      call = CallExpr (result cf) v
      bindStmt = DefVar n call
      stmt = Seq (setup cf) bindStmt
  return $ Compiled { setup = stmt, result = v }

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
  | i1 == i2 = cast x
  | otherwise = lookupEnv i1 remainder
-- | i1 == i2 = trace ("   % lookup v" ++ show i1 ++ 
  --                     ": stored = " ++ show (typeRep x) ++ 
  --                     " | expect = " ++ show (typeRep (Proxy :: Proxy a))) $
  --              cast x

evalExpr :: forall a. Typeable a => CExpression a -> Env -> CValue a
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

evalStmt :: CStatement -> Env -> Env
evalStmt (BindExpr x i y) m = 
  let v  = evalExpr x m
      m' = Extend i v m
  in evalStmt y m'
evalStmt Skip m = m
evalStmt (Seq x y) m =
  let m' = evalStmt x m
  in evalStmt y m'
evalStmt (If cond t e) m =
  let cond' = evalExpr cond m
      condBool = unBool cond'
  in if condBool then evalStmt t m else evalStmt e m
  -- trace (">> if res: " ++ (if condBool then "true" else "false"))
evalStmt (While cond body) m =
  let cond' = evalExpr cond m
  in
    if unBool cond'
    then
      let m' = evalStmt body m
      in evalStmt (While cond body) m'
    else
      m
evalStmt (DefFun (_ :: Proxy a) ifun iparam bodySetup (bodyResult :: CExpression b)) m =
  let fn :: CValue a -> CValue b
      fn arg =
        let m' = evalStmt bodySetup (Extend iparam arg m)
        in evalExpr bodyResult m'
  in Extend ifun (FunV fn) m
evalStmt (DefVar i x) m =
  let v  = evalExpr x m'
      m' = Extend i v m
  in m'
evalStmt (UpdateVar i x) m =
  let m' = Extend i (evalExpr x m) m
  in m'

eval :: (Typeable a, Typeable b) => Compiled (a->b) -> CValue a -> Env -> CValue b
eval x arg m =
  let m' = evalStmt (setup x) m
      FunV fn = evalExpr (result x) m'
  in fn arg

indentStr :: Int -> String
indentStr n = replicate (2 * n) ' '

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
showCValue UnitV = "null"
showCValue (PairV x y) = "(" ++ showCValue x ++ ", " ++ showCValue y ++ ")"
showCValue (FunV _) = error "cannot print function"

showCStmt :: Int -> CStatement -> String
showCStmt indent (UpdateVar i x) = "\n" ++ indentStr indent ++ "v" ++ show i ++ " =~ " ++ showCExpression x
showCStmt indent (If cond t f) =
    "\n" ++ indentStr indent ++ "if " ++ showCExpression cond ++ " {"
    ++  showCStmt (indent + 1) t
    ++ "\n" ++ indentStr indent ++ "} else {"
    ++ showCStmt (indent + 1) f
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent (While cond body) =
    "\n" ++ indentStr indent ++ "while " ++ showCExpression cond ++ " {"
    ++ showCStmt (indent + 1) body
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent (BindExpr x i y) =
    "\n" ++ indentStr indent ++ "let v" ++ show i ++ " = " ++ showCExpression x ++ " in"
    ++ showCStmt (indent + 1) y
showCStmt indent (Seq x y) =
    showCStmt indent x ++ showCStmt indent y
showCStmt indent (DefFun _ ifun iparam stup res) =
    "\n" ++ indentStr indent ++ "function" ++ show ifun ++ " (v" ++ show iparam ++ ") {"
    ++ showCStmt (indent + 1) stup
    ++ "\n" ++ indentStr (indent + 1) ++ "return " ++ showCExpression res
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent (DefVar i f) =  "\n" ++ indentStr indent ++ "v" ++ show i ++ " = " ++ showCExpression f
showCStmt _ Skip = ""

showCExpression :: CExpression a -> String
showCExpression (Var i) = "v" ++ show i
showCExpression (Not x) = "!" ++ showCExpression x
showCExpression (LIntOp op x y) = "(" ++ showCExpression x ++ " " ++ showBinOp op ++ " " ++ showCExpression y ++ ")"
showCExpression (LCmpOp op x y) = "(" ++ showCExpression x ++ " " ++ showCmpOp op ++ " " ++ showCExpression y ++ ")"
showCExpression (Val v) = showCValue v
showCExpression (CallExpr f arg) = showCExpression f ++ "(" ++ showCExpression arg ++ ")"
showCExpression (Prod l r) = "(" ++ showCExpression l ++ "," ++ showCExpression r ++ ")"
showCExpression (Fst p) = showCExpression p ++ "[0]"
showCExpression (Snd p) = showCExpression p ++ "[1]"

main :: IO ()
main = do
    let (nl, c') = NL.translate 0 AL.gcdLang
        cl = evalState (translate nl) c'
        ev = eval cl (PairV (IntV 40) (IntV 30)) Empty
    putStrLn "--- Translating ---"
    putStrLn $ showCStmt 0 (setup cl)
    putStrLn "\n--- Evaluating ---\n"
    putStrLn $ showCValue ev