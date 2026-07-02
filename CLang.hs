{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CLang where
import Data.Dynamic

import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import qualified NamedLang as NL
import Control.Monad.State
import Data.Typeable as DType
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Map as Map

-- ─────────────────────────────────────────────
--  Data Types
-- ─────────────────────────────────────────────

data CParam where
  CParam :: Typeable a => Int -> Proxy a -> CParam

data CArg where
  CArg :: Typeable a => CExpression a -> CArg

data CVal where
  CVal :: Typeable a => CValue a -> CVal
  
type CParams = [CParam]
type CArgMap = Map.Map Int CArg
type CValMap = Map.Map Int CVal

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
  Fst :: (Typeable a, Typeable b) => CExpression (a, b) -> CExpression a
  Snd :: (Typeable a, Typeable b) => CExpression (a, b) -> CExpression b
  -- Lists
  EmptyList :: Typeable a => CExpression [a]
  ConsList :: Typeable a => CExpression a -> CExpression [a] -> CExpression [a]
  HeadList :: Typeable a => CExpression [a] -> CExpression a
  TailList :: Typeable a => CExpression [a] -> CExpression [a]
  IsEmpty :: Typeable a => CExpression [a] -> CExpression Bool
  IndexList :: Typeable a => CExpression [a] -> CExpression Int -> CExpression a

data CStatement a where
  Return :: (Typeable a) => CExpression a -> CStatement a
  Seq :: CStatement a -> CStatement a -> CStatement a
  If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a
  DefFun :: (Typeable a, Typeable b) => Int -> (Int, Proxy a) -> CStatement b -> CStatement b
  DefVar :: Typeable a => Int -> CExpression a -> CStatement b
  UpdateVar :: Typeable a => Int -> CExpression a -> CStatement b
  While :: CExpression Bool -> CStatement a -> CStatement a
  Skip :: CStatement a

data ExecResult a where
  Continue  :: Map.Map Int CVal -> ExecResult a
  ReturnVal :: Typeable a => CValue a -> Map.Map Int CVal -> ExecResult a

-- ─────────────────────────────────────────────
--  Helpers
-- ─────────────────────────────────────────────

fresh :: State Int Int
fresh = do
  n <- get
  modify (+1)
  return n

-- ─────────────────────────────────────────────
--  Translate from NamedLang
-- ─────────────────────────────────────────────

translate :: Typeable a => NL.NamedLang a -> State Int (CStatement a)
translate (NL.Apply f x) = do
  (fs, fE) <- translateSub f
  (xs, xE) <- translateSub x
  return $ foldr Seq (Return (CallExpr fE xE)) (fs ++ xs)
translate (NL.If cond t f) = do
    (cs, condE) <- translateSub cond
    (ts, tE) <- translateSub t
    (fs, fE) <- translateSub f
    case (ts, fs) of
        ([], []) -> return $ foldr Seq (Return (Ternary condE tE fE)) cs -- if both branches return a value
        _ -> do
            ct <- translate t
            cf <- translate f
            return $ foldr Seq (If condE ct cf) cs
translate (NL.Lam arg i f) = do
  cf <- translate f
  ifun <- fresh
  return (unsafeCoerce (DefFun ifun (i, arg) (ensureReturn cf)))
translate (NL.Fix (NL.Lam _ i (NL.Lam targ1 i1 f))) = do
  cf <- translate f
  return (unsafeCoerce (DefFun i (i1, targ1) (ensureReturn cf)))
translate (NL.CaseList l nilCase consCase) = do
  (ns, nilE) <- translateSub nilCase
  consStmt <- translate consCase
  (ls, listE) <- translateSub l
  cId <- fresh
  let callExpr = CallExpr (CallExpr (Var cId) (HeadList listE)) (TailList listE)
      caseBody = If (IsEmpty listE) (foldr Seq (Return nilE) ns) (Return callExpr)
  return $ foldr Seq caseBody (unsafeCoerce (bindResult cId consStmt) : ls)
translate x = do
  (xs, xE) <- translateSub x
  return $ foldr Seq (Return xE) xs

-- Translate a sub-expression. Returns the statements that need to run
-- first, and the expression to use afterwards.
translateSub :: Typeable a => NL.NamedLang a -> State Int ([CStatement b], CExpression a)
translateSub (NL.Var n) = return ([], Var n)
translateSub (NL.LInt n) = return ([], Val (IntV n))
translateSub (NL.LBool b) = return ([], Val (BoolV b))
translateSub NL.EmptyList = return ([], EmptyList)
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

bindResult :: Int -> CStatement a -> CStatement a
bindResult i (Return x) = DefVar i x
bindResult i (Seq x y) = Seq x (bindResult i y)
bindResult i (If c t e) = If c (bindResult i t) (bindResult i e)
bindResult i (While c x) = While c (bindResult i x)
bindResult i def@(DefFun ifun (_, _ :: Proxy arg) (_ :: CStatement b)) =
  let var = Var ifun :: CExpression (arg -> b)
  in Seq def (DefVar i var)
bindResult _ s = s

ensureReturn :: CStatement a -> CStatement a
ensureReturn stmt = case stmt of
  DefFun ifun1 _ _ -> Seq stmt (Return (Var ifun1))
  Seq x y -> Seq x (ensureReturn y)
  _ -> stmt

-- ─────────────────────────────────────────────
--  Replace Bindings
-- ─────────────────────────────────────────────

replaceVarBinding :: CExpression a -> CArgMap -> CExpression a
replaceVarBinding (Var i) m = 
  case Map.lookup i m of
    Just (CArg n) -> replaceVarBinding (unsafeCoerce n) m
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

replaceVarBindingStmt :: CStatement a -> CArgMap -> CStatement a
replaceVarBindingStmt (Seq x y) m = Seq (replaceVarBindingStmt x m) (replaceVarBindingStmt y m)
replaceVarBindingStmt (If cond x y) m = If (replaceVarBinding cond m) (replaceVarBindingStmt x m) (replaceVarBindingStmt y m)
replaceVarBindingStmt (While cond x) m = While (replaceVarBinding cond m) (replaceVarBindingStmt x m)
replaceVarBindingStmt (DefFun ifun param body) m = DefFun ifun param (replaceVarBindingStmt body m)
replaceVarBindingStmt (Return x) m = Return (replaceVarBinding x m)
replaceVarBindingStmt (DefVar i x) m = 
  case Map.lookup i m of
    Just _ -> Skip
    _ -> DefVar i (replaceVarBinding x m)
replaceVarBindingStmt (UpdateVar i x) m = UpdateVar i (replaceVarBinding x m)
replaceVarBindingStmt Skip _ = Skip

collectVarDefs :: CStatement a -> Map.Map Int Int -> Map.Map Int Int
collectVarDefs (DefVar i _) m = Map.insertWith (+) i 1 m
collectVarDefs (Seq x y) m = collectVarDefs x (collectVarDefs y m)
collectVarDefs (If _ x y) m = collectVarDefs x (collectVarDefs y m)
collectVarDefs (While _ x) m = collectVarDefs x m
collectVarDefs (DefFun _ _ body) m = collectVarDefs body m
collectVarDefs _ m = m

-- map stores for each var we have unbound what its value is now
-- so if we had let v8 = v7 in ..., store v8 -> v7 in map
collectBindings :: CStatement a -> CArgMap -> CArgMap
collectBindings (DefVar i x) m = Map.insert i (CArg x) m
collectBindings (Seq x y) m = collectBindings x (collectBindings y m)
collectBindings (If _ x y) m = collectBindings x (collectBindings y m)
collectBindings (While _ x) m = collectBindings x m
collectBindings (DefFun _ _ body) m = collectBindings body m
collectBindings _ m = m

-- remove var definitions for variables that are defined exactly once
-- by replacing references to them with their value
optimizeBindings :: CStatement a -> CStatement a
optimizeBindings stmt =
  let varDefs = collectBindings stmt Map.empty
      varDefCount = collectVarDefs stmt Map.empty
      bindings = Map.filterWithKey (\i _ -> Map.lookup i varDefCount == Just 1) varDefs
  in replaceVarBindingStmt stmt bindings

-- ─────────────────────────────────────────────
--  Evaluation
-- ─────────────────────────────────────────────

unInt :: CValue Int -> Int
unInt (IntV x) = x

unBool :: CValue Bool -> Bool
unBool (BoolV x) = x

unList :: CValue [a] -> [CValue a]
unList (ListV x) = x

lookupEnv :: Int -> CValMap -> Maybe (CValue a)
lookupEnv i m =
  case Map.lookup i m of
    Just (CVal x) -> unsafeCoerce Just x
    _ -> Nothing

evalExpr :: CExpression a -> CValMap -> CValue a
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

evalStmt :: CStatement c -> CValMap -> ExecResult c
evalStmt (DefVar i x) m = Continue (Map.insert i (CVal (evalExpr x m)) m)
evalStmt (UpdateVar i x) m = Continue (Map.insert i (CVal (evalExpr x m)) m)
evalStmt (Return x) m = ReturnVal (evalExpr x m) m
evalStmt Skip m = Continue m
evalStmt (Seq x y) m =
  case evalStmt x m of
    ReturnVal v env' -> ReturnVal v env'
    Continue env' -> evalStmt y env'
evalStmt (If cond t e) m =
  if unBool (evalExpr cond m) then evalStmt t m else evalStmt e m
evalStmt (While cond body) env =
  if unBool (evalExpr cond env)
  then case evalStmt body env of
      ReturnVal v env' -> ReturnVal v env'
      Continue env' -> evalStmt (While cond body) env'
  else Continue env
evalStmt (DefFun ifun (iparam, _ :: Proxy d) (body :: CStatement c)) m =
  let fn :: CValue d -> CValue c
      fn arg = case evalStmt body (Map.insert iparam (CVal arg) m') of
                ReturnVal v _ -> v
                Continue _ -> error "function does not return anything"
      m' = Map.insert ifun (CVal (FunV fn)) m
  in Continue m'

eval :: CStatement a -> CValMap -> CValue a
eval x m = case evalStmt x m of
  Continue _ -> error "Eval did not return anything"
  ReturnVal v _ -> v

-- ─────────────────────────────────────────────
--  Printing
-- ─────────────────────────────────────────────

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
    (h:t) -> showCValue h ++ ", " ++ showCValue (ListV t)

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
showCStmt indent (Seq x y) =
    showCStmt indent x ++ showCStmt indent y
showCStmt indent (DefFun ifun (i, targ) (body :: CStatement b)) =
    "\n" ++ indentStr indent ++ show (DType.typeRep (Proxy :: Proxy b)) ++ " function" ++ show ifun ++ " (" ++ show (DType.typeRep targ) ++ " v" ++ show i ++ ") {"
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
showCExpression (ConsList x l) = "cons(&(" ++ show (typeRep x) ++ "){" ++ showCExpression x ++ "}, " ++ showCExpression l ++ ")"
showCExpression (IsEmpty l) = "isEmpty(" ++ showCExpression l ++ ")"
showCExpression (HeadList l) = "*(" ++ show (typeRep l) ++ ")" ++ "head(" ++ showCExpression l ++ ")"
showCExpression (TailList l) = "tail(" ++ showCExpression l ++ ")"
showCExpression (IndexList l i) = showCExpression l ++ "[" ++ showCExpression i ++ "]"
showCExpression (Ternary cond thn els) = "(" ++ showCExpression cond ++ ") ? (" ++ showCExpression thn ++ ") : (" ++ showCExpression els ++ ")"
