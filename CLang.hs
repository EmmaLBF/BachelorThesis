{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.Map as Map

-- TODO add algebraic datatypes

data CParam where
  CParam :: Typeable a => Int -> Proxy a -> CParam

data CArg where
  CArg :: Typeable a => CExpression a -> CArg

type CParams = [CParam]

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    UnitV :: CValue ()
    FunV  :: (CValue a -> CValue b) -> CValue (a -> b)
    PairV :: CValue a -> CValue b -> CValue (a, b)
    ListV :: [CValue a] -> CValue [a]

data CExpression a where
  Val :: CValue a -> CExpression a
  Not :: CExpression Bool -> CExpression Bool
  Abs :: CExpression Int -> CExpression Int
  Var :: (Typeable a) => Int -> CExpression a
  LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
  LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
  LBoolOp :: AL.BoolOp -> CExpression Bool -> CExpression Bool -> CExpression Bool
  Ternary :: Typeable a => CExpression Bool -> CExpression a -> CExpression a -> CExpression a
  CallExpr :: (Typeable a, Typeable b) => CExpression (a -> b) -> CExpression a -> CExpression b
  -- Tuples
  Prod :: (Typeable a, Typeable b) => CExpression a -> CExpression b -> CExpression (a, b)
  Fst  :: (Typeable a, Typeable b) => CExpression (a, b) -> CExpression a
  Snd  :: (Typeable a, Typeable b) => CExpression (a, b) -> CExpression b
  -- Lists
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
  DefFun    :: (Typeable a, Typeable b) => Proxy b -> Int -> (Int, Proxy a) -> CStatement b -> CStatement b
  DefVar :: Typeable a => Int -> CExpression a -> CStatement b
  UpdateVar :: Typeable a => Int -> CExpression a -> CStatement b
  While :: CExpression Bool -> CStatement a -> CStatement a
  Skip :: CStatement a

data Env where
  Empty :: Env
  Extend :: Typeable a => Int -> CValue a -> Env -> Env

data ExecResult a where
  Continue  :: Env -> ExecResult a
  ReturnVal :: Typeable a => CValue a -> Env -> ExecResult a

fresh :: State Int Int
fresh = do
  n <- get
  modify (+1)
  return n

-- Translate a sub-expression. Returns the statements that need to run
-- first, and the expression to use afterwards.
translateSub :: forall a. Typeable a => NL.NamedLang a -> State Int ([CStatement ()], CExpression a)
translateSub (NL.Var n)    = return ([], Var n)
translateSub (NL.LInt n)   = return ([], Val (IntV n))
translateSub (NL.LBool b)  = return ([], Val (BoolV b))
translateSub NL.EmptyList  = return ([], EmptyList)
translateSub (NL.Fst p) = do
  (ps, pE) <- translateSub p
  return (ps, Fst pE)
translateSub (NL.Snd p) = do
  (ps, pE) <- translateSub p
  return (ps, Snd pE)
translateSub (NL.Not p) = do
  (ps, pE) <- translateSub p
  return (ps, Not pE)
translateSub (NL.Abs p) = do
  (ps, pE) <- translateSub p
  return (ps, Abs pE)
translateSub (NL.Prod x y) = do
  (xs, xE) <- translateSub x
  (ys, yE) <- translateSub y
  return (xs ++ ys, Prod xE yE)
translateSub (NL.ConsList x l) = do
  (xs, xE) <- translateSub x
  (ls, lE) <- translateSub l
  return (xs ++ ls, ConsList xE lE)
translateSub (NL.LIntOp op l r) = do
  (ls, lE) <- translateSub l
  (rs, rE) <- translateSub r
  return (ls ++ rs, LIntOp op lE rE)
translateSub (NL.LCmpOp op l r) = do
  (ls, lE) <- translateSub l
  (rs, rE) <- translateSub r
  return (ls ++ rs, LCmpOp op lE rE)
translateSub (NL.LBoolOp op l r) = do
  (ls, lE) <- translateSub l
  (rs, rE) <- translateSub r
  return (ls ++ rs, LBoolOp op lE rE)
translateSub e = do -- catches all statement translation
  stmt <- translate e
  i <- fresh
  return ([unsafeCoerce (bindResult i stmt)], Var i)

seqAll :: [CStatement ()] -> CStatement a -> CStatement a
seqAll ss final = foldr (Seq . unsafeCoerce) final ss

bindResult :: Int -> CStatement a -> CStatement a
bindResult i (Return x)  = BindExpr x i Skip
bindResult i (Seq x y)   = Seq x (bindResult i y)
bindResult i (If c t e)  = If c (bindResult i t) (bindResult i e)
bindResult i def@(DefFun (_ :: Proxy b) ifun (_, _ :: Proxy arg) _) =
  let var = Var ifun :: CExpression (arg -> b)
  in Seq def (BindExpr var i Skip)
bindResult _ s = s

ensureReturn :: CStatement a -> CStatement a
ensureReturn stmt = case stmt of
  DefFun _ ifun1 _ _ -> Seq stmt (Return (Var ifun1))
  Seq x y -> Seq x (ensureReturn y)
  _ -> stmt

translate :: forall a. Typeable a => NL.NamedLang a -> State Int (CStatement a)
translate (NL.Apply (f :: NL.NamedLang (arg -> a)) (x :: NL.NamedLang arg)) = do
  (fs, fE) <- translateSub f
  (xs, xE) <- translateSub x
  return $ seqAll (fs ++ xs) (Return (CallExpr fE xE))
translate (NL.If cond t f) = do
  (cs, condE) <- translateSub cond
  ct <- translate t
  cf <- translate f
  return $ seqAll cs (If condE ct cf)
translate (NL.Lam arg i (f :: NL.NamedLang b)) = do
  cf <- translate f
  ifun <- fresh
  let body = case cf of
              (DefFun _ ifun1 _ _) -> Seq cf (Return (Var ifun1))
              _ -> cf
  return (unsafeCoerce (DefFun (Proxy :: Proxy b) ifun (i, arg) body))
translate (NL.Fix (NL.Lam _ i (NL.Lam targ1 i1 (f :: NL.NamedLang b)))) = do
  cf <- translate f
  return (unsafeCoerce (DefFun (Proxy :: Proxy b) i (i1, targ1) (ensureReturn (unsafeCoerce cf))))
translate (NL.CaseList l (nilCase :: NL.NamedLang a) (consCase :: NL.NamedLang (a1 -> [a1] -> a))) = do
  (ns, nilE) <- translateSub nilCase
  consStmt <- translate consCase
  (ls, listE) <- translateSub l
  cId <- fresh
  let callExpr = CallExpr (CallExpr (Var cId) (HeadList listE)) (TailList listE)
      caseBody = If (IsEmpty listE) (seqAll ns (Return nilE)) (Return callExpr)
  return $ seqAll (unsafeCoerce (bindResult cId consStmt) : ls)
         $ unsafeCoerce caseBody
translate x = do
  (xs, xE) <- translateSub x
  return $ seqAll xs (Return xE)

replaceVarBinding :: CExpression a -> Map.Map Int CArg -> CExpression a
replaceVarBinding (Var i) m =
  case Map.lookup i m of
    Just (CArg n) -> unsafeCoerce n
    Nothing -> Var i
replaceVarBinding (Not x) m = Not (replaceVarBinding x m)
replaceVarBinding (Abs x) m = Abs (replaceVarBinding x m)
replaceVarBinding (LIntOp op x y) m = LIntOp op (replaceVarBinding x m) (replaceVarBinding y m)
replaceVarBinding (LCmpOp op x y) m = LCmpOp op (replaceVarBinding x m) (replaceVarBinding y m)
replaceVarBinding (LBoolOp op x y) m = LBoolOp op (replaceVarBinding x m) (replaceVarBinding y m)
replaceVarBinding (Ternary x y z) m = Ternary (replaceVarBinding x m) (replaceVarBinding y m) (replaceVarBinding z m)
replaceVarBinding (CallExpr x y) m = CallExpr (replaceVarBinding x m) (replaceVarBinding y m)
replaceVarBinding (Prod x y) m = Prod (replaceVarBinding x m) (replaceVarBinding y m)
replaceVarBinding (Fst x) m = Fst (replaceVarBinding x m)
replaceVarBinding (Snd x) m = Snd (replaceVarBinding x m)
replaceVarBinding (HeadList x) m = HeadList (replaceVarBinding x m)
replaceVarBinding (TailList x) m = TailList (replaceVarBinding x m)
replaceVarBinding (IsEmpty x) m = IsEmpty (replaceVarBinding x m)
replaceVarBinding (IndexList i x) m = IndexList i (replaceVarBinding x m)
replaceVarBinding (ConsList x y) m = ConsList (replaceVarBinding x m) (replaceVarBinding y m)
replaceVarBinding expr _ = expr

replaceVarBindingStmt :: CStatement a -> Map.Map Int CArg -> CStatement a
replaceVarBindingStmt (BindExpr x i y) m =
  BindExpr (replaceVarBinding x m) i (replaceVarBindingStmt y m)
replaceVarBindingStmt (Seq x y) m =
  Seq (replaceVarBindingStmt x m) (replaceVarBindingStmt y m)
replaceVarBindingStmt (If cond x y) m =
  let cond' = replaceVarBinding cond m
      x' = replaceVarBindingStmt x m
      y' = replaceVarBindingStmt y m
  in If cond' x' y'
replaceVarBindingStmt (While cond x) m =
  let cond' = replaceVarBinding cond m
      x' = replaceVarBindingStmt x m
  in While cond' x'
replaceVarBindingStmt (DefFun tret ifun param body) m =
  let body' = replaceVarBindingStmt body m
  in DefFun tret ifun param body'
replaceVarBindingStmt (Return x) m = Return (replaceVarBinding x m)
replaceVarBindingStmt (DefVar i x) m = DefVar i (replaceVarBinding x m)
replaceVarBindingStmt (UpdateVar i x) m = UpdateVar i (replaceVarBinding x m)
replaceVarBindingStmt Skip _ = Skip

-- map stores for each var we have unbound what its value is now
-- so if we had let v8 = v7 in ..., store v8 -> v7 in map
optimizeBindings :: CStatement a -> Map.Map Int CArg -> (CStatement a, Map.Map Int CArg)
optimizeBindings (BindExpr x i y) m =
  let x' = replaceVarBinding x m
  in (y, Map.insert i (CArg x') m)
optimizeBindings (Seq x y) m =
  let (x', m') = optimizeBindings x m
      (y', m'') = optimizeBindings y m'
  in (Seq x' y', m'')
optimizeBindings (If cond x y) m =
  let (x', m') = optimizeBindings x m
      (y', m'') = optimizeBindings y m'
  in (If cond x' y', m'')
optimizeBindings (While cond x) m =
  let (x', m') = optimizeBindings x m
  in (While cond x', m')
optimizeBindings (DefFun tret ifun param body) m =
  let (body', m') = optimizeBindings body m
  in (DefFun tret ifun param body', m')
optimizeBindings x m = (x, m)

-- EVALUATION

unInt :: CValue Int -> Int
unInt (IntV x) = x

unBool :: CValue Bool -> Bool
unBool (BoolV x) = x

unList :: CValue [a] -> [CValue a]
unList (ListV x) = x

lookupEnv :: forall a. Typeable a => Int -> Env -> Maybe (CValue a)
lookupEnv _ Empty = Nothing
lookupEnv i1 (Extend i2 x remainder)
  | i1 == i2 =
      -- trace ("   % lookup v" ++ show i1 ++
      --       ": stored = " ++ show (DType.typeRep x) ++
      --       " | expect = " ++ show (DType.typeRep (Proxy :: Proxy a))) $
                  case cast x of
                    Just v -> v
                    Nothing -> Just (unsafeCoerce x)
  | otherwise = lookupEnv i1 remainder

evalExpr :: forall a. Typeable a => CExpression a -> Env -> CValue a
evalExpr (Val x) _ = x
evalExpr (Var i) m =
  case lookupEnv i m of
    Just v -> v
    Nothing -> error ("Variable not found: " ++ show i)
evalExpr (LIntOp op lhs rhs) m =
  IntV (AL.binop op (unInt (evalExpr lhs m)) (unInt (evalExpr rhs m)))
evalExpr (LBoolOp op lhs rhs) m =
  BoolV (AL.boolop op (unBool (evalExpr lhs m)) (unBool (evalExpr rhs m)))
evalExpr (LCmpOp op lhs rhs) m =
  BoolV (AL.cmpop op (unInt (evalExpr lhs m)) (unInt (evalExpr rhs m)))
evalExpr (CallExpr f arg) m =
  let FunV fn = evalExpr f m
  in fn (evalExpr arg m)
evalExpr (Prod l r) m = PairV (evalExpr l m) (evalExpr r m)
evalExpr (Fst p) m = let PairV x _ = evalExpr p m in x
evalExpr (Snd p) m = let PairV _ x = evalExpr p m in x
evalExpr (Not x) m = BoolV (not (unBool (evalExpr x m)))
evalExpr (Abs x) m = IntV (abs (unInt (evalExpr x m)))
evalExpr EmptyList _ = ListV []
evalExpr (ConsList x l) m = ListV (evalExpr x m : unList (evalExpr l m))
evalExpr (Ternary cond thn els) m = if unBool (evalExpr cond m) then evalExpr thn m else evalExpr els m
evalExpr (HeadList l) m =
  case evalExpr l m of
    ListV [] -> error "List is empty, cannot get head"
    ListV (h:_) -> h
evalExpr (TailList l) m =
  case evalExpr l m of
    ListV [] -> error "List is empty, cannot get tail"
    ListV (_:t) -> ListV t
evalExpr (IsEmpty l) m =
  case evalExpr l m of
    ListV [] -> BoolV True
    _ -> BoolV False
evalExpr (IndexList l i) m =
  let ListV vs = evalExpr l m
      IntV idx = evalExpr i m
  in vs !! idx

evalStmt :: CStatement a -> Env -> ExecResult a
evalStmt (BindExpr x i y) m = evalStmt y (Extend i (evalExpr x m) m)
evalStmt (DefVar i x) m = Continue (Extend i (evalExpr x m) m)
evalStmt (UpdateVar i x) m = Continue (Extend i (evalExpr x m) m)
evalStmt (Return x) m = ReturnVal (evalExpr x m) m
evalStmt Skip m = Continue m
evalStmt (Seq x y) m =
  case evalStmt x m of
    ReturnVal v env' -> ReturnVal v env'
    Continue env'    -> evalStmt y env'
evalStmt (If cond t e) m =
  if unBool (evalExpr cond m) then evalStmt t m else evalStmt e m
evalStmt (While cond body) env =
  if unBool (evalExpr cond env) then
    case evalStmt body env of
      ReturnVal v env' -> ReturnVal v env'
      Continue env'    -> evalStmt (While cond body) env'
  else Continue env
evalStmt (DefFun (_ :: Proxy b) ifun (iparam, _ :: Proxy a) body) m =
  let fn :: CValue a -> CValue b
      fn arg = case evalStmt body (Extend iparam arg m') of
                ReturnVal v _ -> v
                Continue _ -> error "function does not return anything"
      m' = Extend ifun (FunV fn) m
  in Continue m'

eval :: Typeable a => CStatement a -> Env -> CValue a
eval (x :: CStatement a) m = case evalStmt x m of
  Continue _    -> error "Eval did not return anything"
  ReturnVal v _ -> v

-- PRINTING

indentStr :: Int -> String
indentStr n = replicate (2 * n) ' '

showBinOp :: AL.BinOp -> String
showBinOp Plus  = "+"
showBinOp Min   = "-"
showBinOp Times = "*"
showBinOp Div   = "/"
showBinOp Mod   = "%"

showBoolOp :: AL.BoolOp -> String
showBoolOp AL.And  = "&&"
showBoolOp AL.Or   = "||"

showCmpOp :: AL.CmpOp -> String
showCmpOp Eq = "=="
showCmpOp Lt = "<"
showCmpOp Gt = ">"

showCValue :: CValue a -> String
showCValue (IntV n)  = show n
showCValue (BoolV b) = show b
showCValue UnitV = "NULL"
showCValue (PairV x y) = "(" ++ showCValue x ++ ", " ++ showCValue y ++ ")"
showCValue (FunV _) = "funv"
showCValue (ListV l) =
  case l of
    [] -> ""
    [h] -> showCValue h
    (h:t) -> showCValue h ++ ", " ++ showCValue (ListV t)

showProx :: DType.TypeRep -> String
showProx p =
    let args = typeRepArgs p
        con  = show (DType.typeRepTyCon p)
    in case (con, args) of
        ("Int",  [])     -> "int"
        ("Bool", [])     -> "bool"
        ("()",   [])     -> "void*"
        ("[]",   [_])     -> "Node*"
        ("(,)", [_, _]) -> "Pair*"
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
    "Seq " ++ showLitStmt indent x ++ showLitStmt indent y
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
showCStmt indent (DefFun tret ifun (i, targ) body) =
    "\n" ++ indentStr indent ++ showProx (DType.typeRep tret) ++ " function" ++ show ifun ++ " (" ++ showProx (DType.typeRep targ) ++ " v" ++ show i ++ ") {"
    ++ showCStmt (indent + 1) body
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent (DefVar i f) =  "\n" ++ indentStr indent ++ "v" ++ show i ++ " = " ++ showCExpression f ++ ";"
showCStmt indent (Return x) =  "\n" ++ indentStr indent ++ "return " ++ showCExpression x
showCStmt _ Skip = ""

showCExpression :: CExpression a -> String
showCExpression EmptyList = "NULL"
showCExpression (Val v) = showCValue v
showCExpression (Var i) = "v" ++ show i
showCExpression (Abs x) = "|" ++ showCExpression x ++ "|"
showCExpression (Not x) = "!" ++ showCExpression x
showCExpression (Fst p) = showCExpression p ++ ".at(0)"
showCExpression (Snd p) = showCExpression p ++ ".at(1)"
showCExpression (LIntOp op x y) = "(" ++ showCExpression x ++ " " ++ showBinOp op ++ " " ++ showCExpression y ++ ")"
showCExpression (LBoolOp op x y) = "(" ++ showCExpression x ++ " " ++ showBoolOp op ++ " " ++ showCExpression y ++ ")"
showCExpression (LCmpOp op x y) = "(" ++ showCExpression x ++ " " ++ showCmpOp op ++ " " ++ showCExpression y ++ ")"
showCExpression (CallExpr f arg) = showCExpression f ++ "(" ++ showCExpression arg ++ ")"
showCExpression (Prod l r) = "(" ++ showCExpression l ++ "," ++ showCExpression r ++ ")"
showCExpression (ConsList x l) = "cons(&(" ++ showProx (DType.typeRep x) ++ "){" ++ showCExpression x ++ "}, " ++ showCExpression l ++ ")"
showCExpression (IsEmpty l) = "isEmpty(" ++ showCExpression l ++ ")"
showCExpression (HeadList l) = "*(" ++ showProx (DType.typeRep l) ++ ")" ++ "head(" ++ showCExpression l ++ ")"
showCExpression (TailList l) = "tail(" ++ showCExpression l ++ ")"
showCExpression (IndexList l i) = showCExpression l ++ "[" ++ showCExpression i ++ "]"
showCExpression (Ternary cond thn els) = "(" ++ showCExpression cond ++ ") ? (" ++ showCExpression thn ++ ") : (" ++ showCExpression els ++ ")"

-- showEnv :: Env -> String
-- showEnv Empty = ""
-- showEnv (Extend i x r) = "(" ++ show i ++ ": " ++ showCValue x ++ "), " ++ showEnv r

main :: IO ()
main = do
    let (nl, c') = NL.translate 0 AL.mergeSortCall
        cl = evalState (translate nl) c'
        ev = eval cl Empty
    putStrLn $ NL.pretty nl
    putStrLn "--- Translating ---"
    putStrLn $ showCStmt 0 cl
    -- putStrLn $ showLitStmt 0 cl
    putStrLn "--- Opt ---"
    let (opt, newBinds) = optimizeBindings cl Map.empty
    let opt' = replaceVarBindingStmt opt newBinds
    putStrLn $ showCStmt 0 opt'
    let _ = eval opt' Empty
    -- putStrLn $ showCValue evOpt

    putStrLn "\n--- Evaluating ---"
    putStrLn $ showCValue ev