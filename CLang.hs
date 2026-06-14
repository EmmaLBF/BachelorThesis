{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

data CVal where
  CVal :: Typeable a => CValue a -> CVal
  
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
  BindExpr :: Typeable a => CExpression a -> Int -> CStatement b -> CStatement b
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

fresh :: State Int Int
fresh = do
  n <- get
  modify (+1)
  return n

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
bindResult i def@(DefFun ifun (_, _ :: Proxy arg) (_ :: CStatement b)) =
  let var = Var ifun :: CExpression (arg -> b)
  in Seq def (DefVar i var)
bindResult _ s = s

ensureReturn :: CStatement a -> CStatement a
ensureReturn stmt = case stmt of
  DefFun ifun1 _ _ -> Seq stmt (Return (Var ifun1))
  Seq x y -> Seq x (ensureReturn y)
  _ -> stmt

translate :: Typeable a => NL.NamedLang a -> State Int (CStatement a)
translate (NL.Apply f x) = do
  (fs, fE) <- translateSub f
  (xs, xE) <- translateSub x
  return $ foldr Seq (Return (CallExpr fE xE)) (fs ++ xs)
translate (NL.If cond t f) = do
  (cs, condE) <- translateSub cond
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


-- REPLACE BINDINGS

replaceVarBinding :: CExpression a -> State (Map.Map Int CArg) (CExpression a)
replaceVarBinding (Var i) = do
  m <- get
  return $ case Map.lookup i m of
    Just (CArg n) -> unsafeCoerce n
    Nothing -> Var i
replaceVarBinding (Not x) = Not <$> replaceVarBinding x
replaceVarBinding (Abs x) = Abs <$> replaceVarBinding x
replaceVarBinding (LIntOp op x y) = LIntOp op <$> replaceVarBinding x <*> replaceVarBinding y
replaceVarBinding (LCmpOp op x y) = LCmpOp op <$> replaceVarBinding x <*> replaceVarBinding y
replaceVarBinding (LBoolOp op x y) = LBoolOp op <$> replaceVarBinding x <*> replaceVarBinding y
replaceVarBinding (Ternary x y z) = Ternary <$> replaceVarBinding x <*> replaceVarBinding y <*> replaceVarBinding z
replaceVarBinding (CallExpr x y) = CallExpr <$> replaceVarBinding x <*> replaceVarBinding y
replaceVarBinding (Prod x y) = Prod <$> replaceVarBinding x <*> replaceVarBinding y
replaceVarBinding (Fst x) = Fst <$> replaceVarBinding x
replaceVarBinding (Snd x) = Snd <$> replaceVarBinding x
replaceVarBinding (HeadList x) = HeadList <$> replaceVarBinding x
replaceVarBinding (TailList x) = TailList <$> replaceVarBinding x
replaceVarBinding (IsEmpty x) = IsEmpty <$> replaceVarBinding x
replaceVarBinding (IndexList i x) = IndexList i <$> replaceVarBinding x
replaceVarBinding (ConsList x y) = ConsList <$> replaceVarBinding x <*> replaceVarBinding y
replaceVarBinding expr = return expr

replaceVarBindingStmt :: CStatement a -> State (Map.Map Int CArg) (CStatement a)
replaceVarBindingStmt (BindExpr x i y) = do
    x' <- replaceVarBinding x
    y' <- replaceVarBindingStmt y
    return (BindExpr x' i y')
replaceVarBindingStmt (Seq x y) = Seq <$> replaceVarBindingStmt x <*> replaceVarBindingStmt y
replaceVarBindingStmt (If cond x y) = If <$> replaceVarBinding cond <*> replaceVarBindingStmt x <*> replaceVarBindingStmt y
replaceVarBindingStmt (While cond x) = While <$> replaceVarBinding cond <*> replaceVarBindingStmt x
replaceVarBindingStmt (DefFun ifun param body) = DefFun ifun param <$> replaceVarBindingStmt body
replaceVarBindingStmt (Return x) = Return <$> replaceVarBinding x
replaceVarBindingStmt (DefVar i x) = DefVar i <$> replaceVarBinding x
replaceVarBindingStmt (UpdateVar i x) = UpdateVar i <$> replaceVarBinding x
replaceVarBindingStmt Skip = return Skip

-- map stores for each var we have unbound what its value is now
-- so if we had let v8 = v7 in ..., store v8 -> v7 in map
collectBindings :: CStatement a -> State (Map.Map Int CArg) (CStatement a)
collectBindings (BindExpr x i y) = do
  x' <- replaceVarBinding x
  modify (Map.insert i (CArg x'))
  return y
collectBindings (DefVar i x) = do
  x' <- replaceVarBinding x
  modify (Map.insert i (CArg x'))
  return Skip
collectBindings (Seq x y) = Seq <$> collectBindings x <*> collectBindings y
collectBindings (If cond x y) = If cond <$> collectBindings x <*> collectBindings y
collectBindings (While cond x) = While cond <$> collectBindings x
collectBindings (DefFun ifun param body) = DefFun ifun param <$> collectBindings body
collectBindings x = return x

optimizeBindings :: CStatement a -> CStatement a
optimizeBindings stmt =
  let (stmt', bindings) = runState (collectBindings stmt) Map.empty
  in evalState (replaceVarBindingStmt stmt') bindings


-- EVALUATION

unInt :: CValue Int -> Int
unInt (IntV x) = x

unBool :: CValue Bool -> Bool
unBool (BoolV x) = x

unList :: CValue [a] -> [CValue a]
unList (ListV x) = x

lookupEnv :: forall a. Typeable a => Int -> Map.Map Int CVal -> Maybe (CValue a)
lookupEnv i1 m =
  case Map.lookup i1 m of
    Just (CVal x) -> unsafeCoerce Just x
    _ -> Nothing

evalExpr :: forall a. Typeable a => CExpression a -> Map.Map Int CVal -> CValue a
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

evalStmt :: CStatement c -> Map.Map Int CVal -> ExecResult c
evalStmt (BindExpr x i y) m = evalStmt y (Map.insert i (CVal (evalExpr x m)) m)
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

eval :: Typeable a => CStatement a -> Map.Map Int CVal -> CValue a
eval x m = case evalStmt x m of
  Continue _ -> error "Eval did not return anything"
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
showCStmt indent (BindExpr x i y) =
    "\n" ++ indentStr indent ++ "let v" ++ show i ++ " = " ++ showCExpression x ++ " in"
    ++ showCStmt (indent + 1) y
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

main :: IO ()
main = do
    let (nl, fresh') = runState (NL.translate AL.nQueensCall) 0
        cl = evalState (translate nl) fresh'
        ev = eval cl Map.empty
    print nl
    putStrLn "--- Translating ---"
    putStrLn $ showCStmt 0 cl
    putStrLn "--- Opt ---"
    let opt = optimizeBindings cl
    putStrLn $ showCStmt 0 opt
    let _ = eval opt Map.empty
    putStrLn "\n--- Evaluating ---"
    putStrLn $ showCValue ev