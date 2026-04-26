{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CLang where
import Data.Dynamic

import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import qualified NamedLang as NL
import Control.Monad.State
import Data.Typeable as DType
import Debug.Trace
import Unsafe.Coerce (unsafeCoerce)
import Type.Reflection as Refl

-- TODO add lists
-- TODO add algebraic datatypes

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    UnitV :: CValue ()
    FunV  :: (CValue a -> CValue b) -> CValue (a -> b)
    PairV :: CValue a -> CValue b -> CValue (a, b)
    ListV :: [CValue a] -> CValue [a]

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
    Ternary :: CExpression Bool -> CExpression a -> CExpression a -> CExpression a
    EmptyList    :: Typeable a => CExpression [a]
    ConsList   :: Typeable a => CExpression a -> CExpression [a] -> CExpression [a]
    HeadList   :: Typeable a => CExpression [a] -> CExpression a
    TailList   :: Typeable a => CExpression [a] -> CExpression [a]
    IsEmpty  :: Typeable a => CExpression [a] -> CExpression Bool
    IndexList  :: Typeable a => CExpression [a] -> CExpression Int -> CExpression a

data CStatement a where
  Return :: (Typeable a) => CExpression a -> CStatement a
  BindExpr :: Typeable a => CExpression a -> Int -> CStatement b -> CStatement b
  Seq :: CStatement a -> CStatement a -> CStatement a
  If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a
  DefFun    :: (Typeable a, Typeable b)
            => Proxy b      -- return type
            -> Int          -- function id
            -> (Int, Proxy a) -> CStatement b -> CStatement b
  DefVar :: Typeable a => Int -> CExpression a -> CStatement b
  UpdateVar :: Typeable a => Int -> CExpression a -> CStatement b
  While :: CExpression Bool -> CStatement a -> CStatement a
  Skip :: CStatement a

fresh :: State Int Int
fresh = do
  n <- get
  modify (+1)
  return n

defaultVarInit :: forall a. Typeable a => Proxy a -> CExpression a
defaultVarInit _ =
  case eqTypeRep (Refl.typeRep @a) (Refl.typeRep @Int) of
    Just HRefl -> Val (IntV 0)
    Nothing ->
      case eqTypeRep (Refl.typeRep @a) (Refl.typeRep @Bool) of
        Just HRefl -> Val (BoolV False)
        Nothing ->
          error "Unsupported type"

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
translateExpr NL.EmptyList = EmptyList
translateExpr (NL.ConsList x l) = ConsList (translateExpr x) (translateExpr l)
translateExpr _ = error "Expected expression got statement"

-- extract the expression from a Return statement
-- extractReturn :: Typeable a => CStatement -> CExpression a
-- extractReturn (Return e) = e
-- extractReturn (Seq _ s)  = extractReturn s
-- extractReturn _          = error "extractReturn: no return"

translate :: forall a. Typeable a => NL.NamedLang a -> State Int (CStatement a)
translate (NL.Apply (f :: NL.NamedLang (arg -> a)) x) = do
  fStmt <- translate f
  let argExpr = translateExpr x
      fId = case unsafeCoerce fStmt of
              DefFun _ i _ _ -> i
              _ -> error "Called apply without a function"
  return $ Seq (unsafeCoerce fStmt)
         $ Return (CallExpr (Var fId :: CExpression (arg -> a)) argExpr)
  -- case unsafeCoerce fStmt of
  --   DefFun _ fId _ _ ->
  --     return $ Seq (unsafeCoerce fStmt)
  --            $ Return (CallExpr (Var fId :: CExpression (arg -> a)) argExpr)
    -- intermediate result: f translated to Seq(defuns..., Return(CallExpr...))
    -- bind the result to a fresh var and call that
    -- _ -> do
    --   rId <- fresh
    --   let callResult = unsafeCoerce (extractReturn fStmt) :: CExpression (arg -> a)
    --   return $ Seq (unsafeCoerce fStmt)
    --          $ BindExpr callResult rId
    --          $ Return (CallExpr (Var rId :: CExpression (arg -> a)) argExpr)


  --     fId = case unsafeCoerce fStmt of
  --             DefFun _ i _ _ -> i
  --             _ -> error "Called apply without a function"
  -- return $ Seq (unsafeCoerce fStmt)
  --        $ Return (CallExpr (Var fId :: CExpression (arg -> a)) argExpr)
translate (NL.If cond t f) = do
  ct <- translate t
  cf <- translate f
  let cc = translateExpr cond
  return (If cc ct cf)
translate (NL.Lam arg i (f :: NL.NamedLang b)) = do
  cf <- translate f
  ifun <- fresh
  let stmt = case cf of
              (DefFun _ ifun1 _ _) ->
                DefFun (Proxy :: Proxy b) ifun (i, arg)
                  (Seq cf (Return (Var ifun1)))
              _ -> DefFun (Proxy :: Proxy b) ifun (i, arg) cf
  return (unsafeCoerce stmt)
translate (NL.Fix (NL.Lam _ i (NL.Lam targ1 i1 (f :: NL.NamedLang b)))) = do
  cf <- translate f
  return (unsafeCoerce (DefFun (Proxy :: Proxy b) i (i1, targ1) (unsafeCoerce cf)))
translate (NL.CaseList l nilCase consCase) = do
  let cxs      = translateExpr l
      nilExpr  = translateExpr nilCase
  consStmt <- translate consCase
  let fId = case consStmt of
              DefFun _ i _ _ -> i
              _ -> error "not a function"
  let callExpr = CallExpr (CallExpr (Var fId) (HeadList cxs)) (TailList cxs)
  return $ Seq (unsafeCoerce consStmt)
         $ Return (Ternary (IsEmpty cxs) nilExpr callExpr)
  -- cnilStmt <- translate nilCase
  -- hId <- fresh
  -- tId <- fresh
  -- cconsInner <- translate (NL.Apply (NL.Apply consCase (NL.Var hId)) (NL.Var tId))
  -- let cconsStmt = BindExpr (HeadList cxs) hId
  --               $ BindExpr (TailList cxs) tId
  --                 cconsInner
  -- return $ If (IsEmpty cxs) cnilStmt cconsStmt
translate x =
  return $ Return (translateExpr x)

unInt :: CValue Int -> Int
unInt (IntV x) = x

unBool :: CValue Bool -> Bool
unBool (BoolV x) = x

unList :: CValue [a] -> [CValue a]
unList (ListV x) = x

data Env where
  Empty :: Env
  Extend :: Typeable a => Int -> CValue a -> Env -> Env

data ExecResult where
  Continue  :: Env -> ExecResult
  ReturnVal :: Typeable a => CValue a -> Env -> ExecResult

lookupEnv :: forall a. Typeable a => Int -> Env -> Maybe (CValue a)
lookupEnv _ Empty = Nothing
lookupEnv i1 (Extend i2 x remainder)
  | i1 == i2 = trace ("   % lookup v" ++ show i1 ++
                    ": stored = " ++ show (DType.typeRep x) ++
                    " | expect = " ++ show (DType.typeRep (Proxy :: Proxy a))) $
             cast x
  | otherwise = lookupEnv i1 remainder

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
  IntV (AL.binop op (unInt (evalExpr lhs m)) (unInt (evalExpr rhs m)))
evalExpr (LCmpOp op lhs rhs) m =
  BoolV (AL.cmpop op (unInt (evalExpr lhs m)) (unInt (evalExpr rhs m)))
evalExpr (CallExpr f arg) m =
  let FunV fn = evalExpr f m
  in fn (evalExpr arg m)
evalExpr (Prod l r) m = PairV (evalExpr l m) (evalExpr r m)
evalExpr (Fst p) m = let PairV x _ = evalExpr p m in x
evalExpr (Snd p) m = let PairV _ x = evalExpr p m in x
evalExpr (Not x) m = BoolV (not (unBool (evalExpr x m)))
evalExpr EmptyList _ = ListV []
evalExpr (ConsList x l) m = ListV (evalExpr x m : unList (evalExpr l m))
evalExpr (Ternary cond thn els) m = if unBool (evalExpr cond m) then evalExpr thn m else evalExpr els m
evalExpr (HeadList l) m =
  case evalExpr l m of
    ListV [] -> error "List is empty, cannot get head"
    ListV (h:_) -> h
evalExpr (TailList l) m =
  case evalExpr l m of
    ListV [] -> error "List is empty, cannot get head"
    ListV (_:t) -> ListV t
evalExpr (IsEmpty l) m =
  case evalExpr l m of
    ListV [] -> BoolV True
    _ -> BoolV False
evalExpr (IndexList l i) m =
  let ListV vs = evalExpr l m
      IntV idx = evalExpr i m
  in vs !! idx

evalStmt :: CStatement a -> Env -> ExecResult
evalStmt (BindExpr x i y) m = evalStmt y (Extend i (evalExpr x m) m)
evalStmt (DefVar i x) m = Continue (Extend i (evalExpr x m) m)
evalStmt (UpdateVar i x) m = Continue (Extend i (evalExpr x m) m)
evalStmt (Return (x :: CExpression a)) m = ReturnVal (evalExpr x m) m
evalStmt Skip m = Continue m
evalStmt (Seq x y) m =
  case evalStmt x m of
    ReturnVal v env' -> ReturnVal v env'
    Continue env'    -> evalStmt y env'
evalStmt (If cond t e) m =
  let condBool = unBool (evalExpr cond m)
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
evalStmt (DefFun (tfun :: Proxy b) ifun (iparam, tparam :: Proxy a) body) m =
  let fn :: CValue a -> CValue b
      fn arg = case evalStmt body (Extend iparam arg m') of
                ReturnVal v _ -> trace ("returning: " ++ show (DType.typeRep v)) $ case cast v of
                  Just v' -> v'
                  Nothing -> error "Type mismatch in DefFun "
                Continue _ -> error "function does not return anything"
      m' = Extend ifun (FunV fn) m
  in trace ("c: " ++ show (DType.typeRep tparam) ++ " | b: " ++ show (DType.typeRep tfun)) $ Continue m'


data SomeCValue where
  SomeCValue :: Typeable a => CValue a -> SomeCValue

eval :: CStatement a -> Env -> SomeCValue
eval x m = case evalStmt x m of
  Continue _    -> error "Eval did not return anything"
  ReturnVal v _ -> SomeCValue v

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

showSomeValue :: SomeCValue -> String
showSomeValue (SomeCValue v) = showCValue v

showCValue :: CValue a -> String
showCValue (IntV n)  = show n
showCValue (BoolV b) = show b
showCValue UnitV = "null"
showCValue (PairV x y) = "(" ++ showCValue x ++ ", " ++ showCValue y ++ ")"
showCValue (FunV _) = "funv"
showCValue (ListV l) =
  case l of
    [] -> ""
    (h:t) -> showCValue h ++ ", " ++ showCValue (ListV t)

showProx :: DType.TypeRep -> String
showProx p =
    let args = typeRepArgs p
        con  = show (DType.typeRepTyCon p)
    in case (con, args) of
        ("Int",  [])     -> "int"
        ("Bool", [])     -> "bool"
        ("()",   [])     -> "void*"
        ("(,)",  [a, _]) -> showProx a ++ "*"
        ("->",   [a, b]) -> showProx b ++ " (*)(" ++ showProx a ++ ")"
        _                -> show p

showLitStmt :: Int -> CStatement a -> String
showLitStmt indent (UpdateVar i x) = "\n" ++ indentStr indent ++ "UpdateVar " ++ show i ++ " = " ++ showCExpression x
showLitStmt indent (If cond t f) =
    "\n" ++ indentStr indent ++ "If " ++ showCExpression cond ++ " {"
    ++  showLitStmt (indent + 1) t
    ++ "\n" ++ indentStr indent ++ "} else {"
    ++ showLitStmt (indent + 1) f
    ++ "\n" ++ indentStr indent ++ "}"
showLitStmt indent (While cond body) =
    "\n" ++ indentStr indent ++ "While " ++ showCExpression cond ++ " {"
    ++ showLitStmt (indent + 1) body
    ++ "\n" ++ indentStr indent ++ "}"
showLitStmt indent (BindExpr x i y) =
    "\n" ++ indentStr indent ++ "BindExpr v" ++ show i ++ " = " ++ showCExpression x ++ "in"
    ++ showLitStmt (indent + 1) y
showLitStmt indent (Seq x y) =
    showLitStmt indent x ++ showLitStmt indent y
showLitStmt indent (DefFun tret ifun (iparam, tparam) body) =
    "\n" ++ indentStr indent ++ "DefFun " ++ show ifun ++ " | tfun: " ++ show (DType.typeRep tret) ++ " | param: (" ++ show (DType.typeRep tparam) ++ " v" ++ show iparam ++ ") {"
    ++ showLitStmt (indent + 1) body
    ++ "\n" ++ indentStr indent ++ "}"
showLitStmt indent (DefVar i f) =  "\n" ++ indentStr indent ++ "v" ++ show i ++ " = " ++ showCExpression f
showLitStmt indent (Return x) =  "\n" ++ indentStr indent ++ "Return " ++ showCExpression x
showLitStmt _ Skip = "Skip"

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
showCStmt indent (DefFun tret ifun (iparam, tparam) body) =
    "\n" ++ indentStr indent ++ showProx (DType.typeRep tret) ++ " function" ++ show ifun ++ " (" ++ showProx (DType.typeRep tparam) ++ " v" ++ show iparam ++ ") {"
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
showCExpression EmptyList = "[]"
showCExpression (ConsList x l) = showCExpression x ++ ":" ++ showCExpression l
showCExpression (IsEmpty l) = "isEmpty(" ++ showCExpression l ++ ")"
showCExpression (HeadList l) = "head(" ++ showCExpression l ++ ")"
showCExpression (TailList l) = "tail(" ++ showCExpression l ++ ")"
showCExpression (IndexList l i) = showCExpression l ++ "[" ++ showCExpression i ++ "]"
showCExpression (Ternary cond thn els) = "(" ++ showCExpression cond ++ ") ? (" ++ showCExpression thn ++ ") : (" ++ showCExpression els ++ ")"

showEnv :: Env -> String
showEnv Empty = ""
showEnv (Extend i x r) = "(" ++ show i ++ ": " ++ showCValue x ++ "), " ++ showEnv r

main :: IO ()
main = do
    let (nl, c') = NL.translate 0 AL.sumListCall
        cl = evalState (translate nl) c'
        ev = eval cl Empty
    putStrLn $ NL.pretty nl
    putStrLn "--- Translating ---"
    putStrLn $ showCStmt 0 cl
    putStrLn "\n--- Evaluating ---"
    putStrLn $ showSomeValue ev