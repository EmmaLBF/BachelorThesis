{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Eta reduce" #-}
{- HLINT ignore "Use first" -}

module CLang2 where
import Data.Dynamic

import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import qualified NamedLang as NL
import Control.Monad.State
import Data.Typeable as DType
import Debug.Trace
import Unsafe.Coerce (unsafeCoerce)
import Type.Reflection as Refl

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    UnitV :: CValue ()
    FunV  :: (CValue a -> CValue b) -> CValue (a -> b)
    PairV :: CValue a -> CValue b -> CValue (a, b)

-- data CParams a where
--   PEmpty :: CParams ()
--   PExtend :: Typeable a => Proxy a -> Int -> CParams b -> CParams (a, b)

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

-- TODO add lists
-- TODO add algebraic datatypes

data CStatement a where
  Return :: CExpression a -> CStatement a
  BindExpr :: Typeable a => CExpression a -> Int -> CStatement b -> CStatement b
  Seq :: CStatement a -> CStatement a -> CStatement a
  If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a
  -- DefFun :: (Typeable a, Typeable b) => Proxy a -> Int -> Int -> CStatement -> CExpression b -> CStatement
  -- DefFun    :: (Typeable a, Typeable b)
  --           => Proxy b      -- return type
  --           -> Int          -- function id
  --           -> (Int, Proxy a)     -- parameter with type
  --           -> CStatement c   -- body setup
  --           -> CExpression b -- body result
  --           -> CStatement c
  DefFun    :: (Typeable a, Typeable b)
            => Proxy b      -- return type
            -> Int          -- function id
            -> (Int, Proxy a) -> CStatement b -> CStatement b
  DefVar :: Typeable a => Int -> CExpression a -> CStatement b
  UpdateVar :: Typeable a => Int -> CExpression a -> CStatement b
  While :: CExpression Bool -> CStatement a -> CStatement a
  Skip :: CStatement ()

fresh :: State Int Int
fresh = do
  n <- get
  modify (+1)
  return n

-- data Compiled a = Compiled
--   { setup :: CStatement a
--   , result :: CExpression a
--   }

-- onlyExpr :: CExpression a -> State Int (Compiled a)
-- onlyExpr expr = return $ Compiled { setup = Skip, result = expr }

defaultVarInit :: forall a. Typeable a => Proxy a -> CExpression a
defaultVarInit _ =
  case eqTypeRep (Refl.typeRep @a) (Refl.typeRep @Int) of
    Just HRefl -> Val (IntV 0)
    Nothing ->
      case eqTypeRep (Refl.typeRep @a) (Refl.typeRep @Bool) of
        Just HRefl -> Val (BoolV False)
        Nothing ->
          error $ "Unsupported type"

translateExpr :: NL.NamedLang a -> CExpression a
translateExpr (NL.LInt n) = Val (IntV n)
translateExpr (NL.LBool b) = Val (BoolV b)
translateExpr (NL.Var n) = Var n
translateExpr (NL.Prod (x :: NL.NamedLang a) y) = Prod (translateExpr x) (translateExpr y)
translateExpr (NL.Fst p) = Fst (translateExpr p)
translateExpr (NL.Snd p) = Snd (translateExpr p)
translateExpr (NL.LIntOp op l r) = LIntOp op (translateExpr l) (translateExpr r)
translateExpr (NL.LCmpOp op l r) = LCmpOp op (translateExpr l) (translateExpr r)
translateExpr (NL.Apply f x) = CallExpr (translateExpr f) (translateExpr x)
translateExpr _ = error "Expected expression got statement"

translate :: forall a. NL.NamedLang a -> State Int (CStatement a)
translate (NL.If cond t f) = do
  ct <- translate t
  cf <- translate f
  let cc = translateExpr cond
  return (If cc ct cf)
  -- let stmt = Seq (DefVar n (defaultVarInit (Proxy :: Proxy a)))
  --                  (If cc
  --                      (Seq (setup ct) (UpdateVar n (result ct)))
  --                      (Seq (setup cf) (UpdateVar n (result cf))))
  -- return $ Compiled { setup = stmt, result = Var n }
translate (NL.Lam arg i (f :: NL.NamedLang b)) = do
  cf <- translate f
  ifun <- fresh
  let stmt = DefFun (Proxy :: Proxy b) ifun (i, arg) cf
  return (unsafeCoerce stmt)
translate (NL.Fix (NL.Lam targ i (NL.Lam targ1 i1 f))) = do
  cf <- translate f
  -- n <- fresh
  return (unsafeCoerce (DefFun targ i (i1, targ1) (unsafeCoerce cf)))
translate x = do
  return $ Return (translateExpr x)

-- translate (NL.Lam arg i (f :: NL.NamedLang b)) = do
--   cf <- translate f
--   ifun <- fresh
--   let rcf = result cf
--       stmt = DefFun (Proxy :: Proxy b) ifun (PExtend arg i PEmpty) (setup cf) (result cf)
--   return $ Compiled { setup = stmt, result = Var ifun }
-- translate (NL.Fix f) = do
--   cf <- translate f
--   n <- fresh
--   let v = Var n :: CExpression a
--       call = CallExpr (result cf) v
--       bindStmt = DefVar n call
--       stmt = Seq (setup cf) bindStmt
--   return $ Compiled { setup = stmt, result = v }

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
                    ": stored = " ++ show (DType.typeRep x) ++
                    " | expect = " ++ show (DType.typeRep (Proxy :: Proxy a))) $
             cast x
  | otherwise = lookupEnv i1 remainder
-- | i1 == i2 = cast x

lookupEnvType :: Int -> Env -> Maybe DType.TypeRep
lookupEnvType _ Empty = Nothing
lookupEnvType i1 (Extend i2 x remainder)
  | i1 == i2  = Just (DType.typeRep x)
  | otherwise = lookupEnvType i1 remainder

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
  in fn (evalExpr arg m)
evalExpr (Prod l r) m = PairV (evalExpr l m) (evalExpr r m)
evalExpr (Fst p) m = let PairV x _ = evalExpr p m in x
evalExpr (Snd p) m = let PairV _ x = evalExpr p m in x
evalExpr (Not x) m = BoolV (not (unBool (evalExpr x m)))

-- bindParams :: CParams a -> CValue a -> Env -> Env
-- bindParams PEmpty UnitV env = env
-- bindParams (PExtend _ i rest) (PairV x xs) env = bindParams rest xs (Extend i x env)

data ExecResult a
  = Continue Env
  | ReturnVal (CValue a) Env

evalStmt :: forall a. Typeable a => CStatement a -> Env -> ExecResult a
evalStmt (BindExpr x i y) m =
  let v  = evalExpr x m
      m' = Extend i v m
  in evalStmt y m'
evalStmt Skip m = Continue m
evalStmt (Seq x y) m =
  case evalStmt x m of
    ReturnVal v env' -> ReturnVal v env'
    Continue env'    -> evalStmt y env'
evalStmt (If cond t e) m =
  let cond' = evalExpr cond m
      condBool = unBool cond'
  in if condBool then evalStmt t m else evalStmt e m
  -- trace (">> if res: " ++ (if condBool then "true" else "false"))
evalStmt (While cond body) env =
  let cond' = evalExpr cond env
  in
    if unBool cond'
    then
      case evalStmt body env of
        ReturnVal v env' -> ReturnVal v env'
        Continue env'    -> evalStmt (While cond body) env'
    else Continue env
-- evalStmt (DefFun (retProx :: Proxy b) ifun (params :: CParams a) bodySetup (bodyResult :: CExpression b)) m =
--   let fn :: CValue a -> CValue b
--       fn (arg :: CValue a) =
--           let m'  = bindParams params arg m
--               m'' = evalStmt bodySetup m'
--           in evalExpr bodyResult m''      
--       env' = Extend ifun (FunV fn) m
--   in env'
evalStmt (DefFun (_ :: Proxy b) ifun (iparam, _) body) m =
  let fn :: CValue a -> CValue b
      fn arg =
        case evalStmt body (Extend iparam arg m) of
          ReturnVal v _ -> v
          Continue _    -> error $ "function does not return anything"
  in Continue (Extend ifun (FunV fn) m)
evalStmt (DefVar i x) m =
  let v  = evalExpr x m'
      m' = Extend i v m
  in Continue m'
evalStmt (UpdateVar i x) m =
  let m' = Extend i (evalExpr x m) m
  in Continue m'
evalStmt (Return x) m = ReturnVal (evalExpr x m) m

eval :: Typeable a => CStatement a -> Env -> CValue a
eval x m = case evalStmt x m of
  Continue _ -> error $ "Eval did not return anything"
  ReturnVal cv _ -> cv
  -- let m' = evalStmt x m
  --     FunV fn = evalExpr (result x) m'
  -- in fn arg

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
showCValue (FunV _) = "funv"

-- showCParam :: CParams -> String
-- showCParam (CParams (prox :: Proxy a) i) = show (typeRep prox) ++ " v" ++ show i

-- showParams :: CParams a -> String
-- showParams PEmpty = ""
-- showParams (PExtend prox i PEmpty) = show prox ++ " v" ++ show i
-- showParams (PExtend prox i r) = show prox ++ " v" ++ show i ++ ", " ++ showParams r
-- showParams (i:r) = showCParam i ++ ", " ++ showParams r

showProx :: DType.TypeRep -> String
showProx p =
    let args = typeRepArgs p
        con  = show (DType.typeRepTyCon p)
    in case (con, args) of
        ("Int",  [])     -> "int"
        ("Bool", [])     -> "bool"
        ("()",   [])     -> "void*"
        ("(,)",  [a, _]) -> showProx a ++ "*"  -- simplification, same as before
        ("->",   [a, b]) -> showProx b ++ " (*)(" ++ showProx a ++ ")"
        _                -> show p

showCStmt :: Int -> CStatement a -> String
showCStmt indent (UpdateVar i x) = "\n" ++ indentStr indent ++ "v" ++ show i ++ " =~ " ++ showCExpression x ++ ";"
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
showCStmt indent (DefFun prox ifun (iparam, tparam) body) =
    "\n" ++ indentStr indent ++ showProx (DType.typeRep prox) ++ " function" ++ show ifun ++ " (" ++ showProx (DType.typeRep tparam) ++ " v" ++ show iparam ++ ") {"
    ++ showCStmt (indent + 1) body
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent (DefVar i f) =  "\n" ++ indentStr indent ++ "v" ++ show i ++ " = " ++ showCExpression f ++ ";"
showCStmt indent (Return x) =  "\n" ++ indentStr indent ++ "return " ++ showCExpression x
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

showEnv :: Env -> String
showEnv Empty = ""
showEnv (Extend i x r) = "(" ++ show i ++ ": " ++ showCValue x ++ "), " ++ showEnv r

main :: IO ()
main = do
    -- let (nl, c') = NL.translate 0 AL.gcdLang
    --     cl = evalState (translate nl) c'
    --     ev = eval cl (PairV (IntV 40) (IntV 30)) Empty
    let (nl, c') = NL.translate 0 AL.fac
        cl = evalState (translate nl) c'
        ev = eval cl Empty
    putStrLn "--- Translating ---"
    putStrLn $ showCStmt 0 cl
    putStrLn "\n--- Evaluating ---"
    putStrLn $ showCValue ev