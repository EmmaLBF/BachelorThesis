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

indentStr :: Int -> String
indentStr n = replicate (2 * n) ' '

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    UnitV :: CValue ()
    FunV  :: (CValue a -> CValue b) -> CValue (a -> b)
    PairV :: CValue a -> CValue b -> CValue (a, b)

-- deriving instance Typeable a => Typeable (CValue a)

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

-- in theory statements shouldn't have a type? since they don't return anything?
-- data CStatement a where
--   Return :: Typeable a => CExpression a -> CStatement a
--   Bind :: Typeable a => CStatement a -> Int -> CStatement b -> CStatement b
--   Seq :: CStatement a -> CStatement b -> CStatement b
--   If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a

--   -- Do not return anything
--   DefFun :: (Typeable a, Typeable b) => Proxy a -> Int -> Int -> CStatement b -> CStatement ()
--   DefVar :: Typeable a => Int -> CExpression a -> CStatement ()
--   UpdateVar :: Typeable a => Int -> CExpression a -> CStatement ()
--   While :: CExpression Bool -> CStatement a -> CStatement ()
data CStatement where
  -- Return :: Typeable a => CExpression a -> CStatement
  BindExpr :: Typeable a => CExpression a -> Int -> CStatement -> CStatement
  -- Bind :: CStatement -> Int -> CStatement -> CStatement
  Seq :: CStatement -> CStatement -> CStatement
  If :: CExpression Bool -> CStatement -> CStatement -> CStatement

  -- Do not return anything
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

type Trans a = State Int (Compiled a)

onlyExpr :: CExpression a -> Trans a
onlyExpr expr = return $ Compiled { setup = Skip, result = expr }

-- toStatement :: Typeable a => Compiled a -> CStatement
-- toStatement c = Seq (setup c) (Return (result c))

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
      -- ret = Return (Var n `asTypeOf` result ct) -- should work since both branches have the same type?
      -- stmt = Bind branch n ret
  return $ Compiled { setup = stmt, result = Var n }
translate (NL.Lam arg i (f :: NL.NamedLang b)) = do
  cf <- translate f
  ifun <- fresh
  let stmt = DefFun arg ifun i (setup cf) (result cf)
  return $ Compiled { setup = stmt, result = Var ifun }
translate (NL.Fix f) = do
  cf <- translate f
  n <- fresh  -- this must not clash with any Lam parameter
  let v = Var n :: CExpression a
      call = CallExpr (result cf) v
      bindStmt = DefVar n call
      stmt = Seq (setup cf) bindStmt
      -- stmt = Seq  (Seq (setup cf) (Return call)) (Return (Val UnitV))
  return $ Compiled { setup = stmt, result = v }
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
  | i1 == i2 = trace ("   % lookup v" ++ show i1 ++ 
                      ": stored = " ++ show (typeRep x) ++ 
                      " | expect = " ++ show (typeRep (Proxy :: Proxy a))) $
               cast x
  | otherwise = lookupEnv i1 remainder

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

-- type Eval a = State Int (EvalValue, Map Int EvalValue)

evalStmt :: CStatement -> Env -> Env
-- evalStmt (Return (x :: CExpression a)) m = (Just (evalExpr x m), m)
evalStmt (BindExpr x i y) m = 
  let v  = evalExpr x m
      m' = Extend i v m
  in evalStmt y m'
evalStmt Skip m = m
  -- let x' = evalExpr
  -- case trace ">> bind x" $ evalStmt x m of
  --     (Nothing, _) -> error $ "no value returned"
      -- (Just (x' :: CValue a), m') -> evalStmt y (trace (">> bind extend v" ++ show i ++ " | type: " ++ show (typeRep x')) $ Extend i x' m')
  -- in trace ">> bind y" $ evalStmt y m''
evalStmt (Seq x y) m =
  let m' = trace (">> seq x" ++ showCStmt 3 x m) $ evalStmt x m
  in trace (">> seq y " ++ showCStmt 3 y m) $ evalStmt y m'
evalStmt (If cond t e) m =
  let cond' = trace ">> if cond" $ evalExpr cond m
      condBool = unBool cond'
  in if trace (">> if res: " ++ (if condBool then "true" else "false")) condBool then evalStmt t m else evalStmt e m
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

showCStmt :: Int -> CStatement -> Env -> String
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
showCStmt indent (BindExpr x i y) m =
    "\n" ++ indentStr indent ++ "let v" ++ show i ++ " = " ++ showCExpression (indent + 1) x m ++ " in"
    ++ showCStmt (indent + 1) y m
showCStmt indent (Seq x y) m =
    showCStmt indent x m ++ showCStmt indent y m
showCStmt indent (DefFun _ ifun iparam stup res) m =
    "\n" ++ indentStr indent ++ "function" ++ show ifun ++ " (v" ++ show iparam ++ ") {"
    ++ showCStmt (indent + 1) stup m
    ++ "\n" ++ indentStr (indent + 1) ++ "return " ++ showCExpression indent res m
    ++ "\n" ++ indentStr indent ++ "}"
-- showCStmt indent (Return x) m =  "\n" ++ indentStr indent ++ "[Return " ++ showCExpression indent x m ++ "]"
showCStmt indent (DefVar i f) m =  "\n" ++ indentStr indent ++ "v" ++ show i ++ " = " ++ showCExpression indent f m
showCStmt _ Skip _ = ""

showCExpression :: Int -> CExpression a -> Env -> String
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
    putStrLn "--- Translating ---"
    putStrLn $ showCStmt 0 (setup cl) Empty
    putStrLn "\n--- Evaluating ---\n"
    putStrLn $ showCValue ev