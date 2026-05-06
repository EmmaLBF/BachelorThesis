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

-- TODO add algebraic datatypes

data CParam where
  CParam :: Typeable a => Int -> Proxy a -> CParam

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
  Var :: (Typeable a) => Int -> CExpression a
  LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
  LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
  Ternary :: CExpression Bool -> CExpression a -> CExpression a -> CExpression a
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

data CallTarget a
  = FunVar Int
  | PrevCall (CExpression a)

getCallTarget :: CStatement a -> CallTarget a
getCallTarget (DefFun _ i _ _)              = FunVar i
getCallTarget (Seq _ y)                     = getCallTarget (unsafeCoerce y)
getCallTarget (Return (CallExpr f arg))     = PrevCall (unsafeCoerce (CallExpr f arg))
getCallTarget (Return (Var i)) = FunVar i
getCallTarget _                             = error "not call target"

removeLastReturn :: CStatement a -> CStatement a
removeLastReturn (Seq x (Return _)) = x
removeLastReturn (Seq x y)          = Seq x (removeLastReturn (unsafeCoerce y))
removeLastReturn (Return _)         = Skip
removeLastReturn x                  = x

ensureReturn :: CStatement a -> CStatement a
ensureReturn stmt = case stmt of
  DefFun _ ifun1 _ _ -> Seq stmt (Return (Var ifun1))
  Seq x y -> Seq x (ensureReturn y)
  _ -> stmt

translateExpr :: NL.NamedLang a -> CExpression a
translateExpr (NL.Var n) = Var n
translateExpr  NL.EmptyList = EmptyList
translateExpr (NL.LInt n) = Val (IntV n)
translateExpr (NL.LBool b) = Val (BoolV b)
translateExpr (NL.Fst p) = Fst (translateExpr p)
translateExpr (NL.Snd p) = Snd (translateExpr p)
translateExpr (NL.Prod x y) = Prod (translateExpr x) (translateExpr y)
translateExpr (NL.Apply f x) = CallExpr (translateExpr f) (translateExpr x)
translateExpr (NL.ConsList x l) = ConsList (translateExpr x) (translateExpr l)
translateExpr (NL.LIntOp op l r) = LIntOp op (translateExpr l) (translateExpr r)
translateExpr (NL.LCmpOp op l r) = LCmpOp op (translateExpr l) (translateExpr r)
translateExpr _ = error "Expected expression got statement"

bindResult :: Int -> CStatement a -> CStatement a
bindResult i (Return x)  = BindExpr x i Skip
bindResult i (Seq x y)   = Seq x (bindResult i y)
bindResult i (If c t e)  = If c (bindResult i t) (bindResult i e)
bindResult _ s            = s

translate :: forall a. Typeable a => NL.NamedLang a -> State Int (CStatement a)
translate (NL.Apply (f :: NL.NamedLang (arg -> a)) (x :: NL.NamedLang arg)) = do
  fStmt <- translate f  -- :: CStatement (arg -> a)
  xStmt <- translate x  -- :: CStatement arg
  -- trace ("=== Apply ===\nfStmt: " ++ showCStmt 0 fStmt ++ "\nxStmt: " ++ showCStmt 0 xStmt) $
  case (fStmt, xStmt) of
    -- f is a function def, x is a plain expr
    (DefFun _ fId _ _, Return xExpr) ->
      return $ Seq (unsafeCoerce fStmt)
             $ Return (CallExpr (Var fId :: CExpression (arg -> a)) xExpr)
    -- f is a function def, x is also a function def
    (DefFun _ fId _ _, DefFun _ xId _ _) ->
      return $ Seq (unsafeCoerce fStmt)
             $ Seq (unsafeCoerce xStmt)
             $ Return (CallExpr (Var fId :: CExpression (arg -> a)) (Var xId :: CExpression arg))
    -- f is a function def, x needs hoisting
    (DefFun _ fId _ _, _) -> do
      xId <- fresh
      return $ Seq (unsafeCoerce fStmt)
             $ Seq (unsafeCoerce (bindResult xId xStmt))
             $ Return (CallExpr (Var fId :: CExpression (arg -> a)) (Var xId :: CExpression arg))
    -- f is a plain expr, x is a plain expr
    (Return fExpr, Return xExpr) ->
      return $ Return (CallExpr fExpr xExpr)
    -- f is a plain expr, x needs hoisting
    (Return fExpr, _) -> do
      xId <- fresh
      return $ Seq (unsafeCoerce (bindResult xId xStmt))
             $ Return (CallExpr fExpr (Var xId :: CExpression arg))
    -- f needs hoisting, x is a plain expr
    (_, Return xExpr) -> do
      fId <- fresh
      return $ Seq (unsafeCoerce (bindResult fId fStmt))
             $ Return (CallExpr (Var fId :: CExpression (arg -> a)) xExpr)
    -- both need hoisting
    (_, _) -> do
      fId <- fresh
      xId <- fresh
      return $ Seq (unsafeCoerce (bindResult fId fStmt))
             $ Seq (unsafeCoerce (bindResult xId xStmt))
             $ Return (CallExpr (Var fId :: CExpression (arg -> a)) (Var xId :: CExpression arg))


  -- fStmt <- translate f
  -- xStmt <- translate x
  -- let target  = getCallTarget (unsafeCoerce fStmt)
  --     seqDefs = case target of
  --                 FunVar _   -> 
  --                   case unsafeCoerce fStmt of
  --                     Return _ -> unsafeCoerce Skip  -- pure expr, nothing to hoist
  --                     s        -> s
  --                 PrevCall _ -> removeLastReturn (unsafeCoerce fStmt)
  --     mkCall :: CExpression arg -> CExpression a
  --     mkCall argExpr = 
  --       case target of
  --         FunVar i   -> CallExpr (Var i) argExpr
  --         PrevCall c -> CallExpr c argExpr
  -- case xStmt of
  --   DefFun _ xId _ _ ->
  --     return $ Seq seqDefs (Seq (unsafeCoerce xStmt) (Return (mkCall (Var xId))))
  --   Return xExpr ->
  --     return $ Seq seqDefs (Return (mkCall (unsafeCoerce xExpr)))
  --   _ -> do
  --     xId <- fresh
  --     return $ Seq (bindResult xId (unsafeCoerce xStmt))
  --           $ Seq seqDefs
  --           $ Return (mkCall (Var xId))

    -- _ -> let argExpr = translateExpr x
    --      in return $ Seq seqDefs
    --                $ Return (mkCall argExpr)
translate (NL.If cond t f) = do
  ct <- translate t
  cf <- translate f
  let cc = translateExpr cond
  return (If cc ct cf)
translate (NL.Lam arg i (f :: NL.NamedLang b)) = do
  cf <- translate f
  ifun <- fresh
  let body = case cf of
              (DefFun _ ifun1 _ _) -> Seq cf (Return (Var ifun1))
              _ -> cf
      def = DefFun (Proxy :: Proxy b) ifun (i, arg) body
  return (unsafeCoerce def)
translate (NL.Fix (NL.Lam _ i (NL.Lam targ1 i1 (f :: NL.NamedLang b)))) = do
  cf <- translate f
  return (unsafeCoerce (DefFun (Proxy :: Proxy b) i (i1, targ1) (ensureReturn (unsafeCoerce cf))))
translate (NL.CaseList l (nilCase :: NL.NamedLang a) (consCase :: NL.NamedLang (a1 -> [a1] -> a))) = do
  let nilExpr  = translateExpr nilCase
  consStmt <- translate consCase
  lId      <- fresh
  lStmt    <- translate l
  let fId = case consStmt of
              DefFun _ i _ _ -> i
              _ -> error "not a function"
      listVar = Var lId :: CExpression [a1]
      callExpr = CallExpr (CallExpr (Var fId) (HeadList listVar)) (TailList listVar)
      caseBody = Return (Ternary (IsEmpty listVar) nilExpr callExpr)
      lHoisted = bindResult lId (unsafeCoerce lStmt)
  return $ Seq (unsafeCoerce consStmt)
         $ Seq (unsafeCoerce lHoisted)
         $ unsafeCoerce caseBody
  -- return $ Seq (unsafeCoerce consStmt)
  --        $ BindExpr cxs lId
  --        $ Return (Ternary (IsEmpty (Var lId)) nilExpr callExpr)
translate x = return $ Return (translateExpr x)

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

showProxList :: DType.TypeRep -> String
showProxList p =
    let args = typeRepArgs p
        con  = show (DType.typeRepTyCon p)
    in case (con, args) of
        ("Int",  [])     -> "int"
        ("Bool", [])     -> "bool"
        ("()",   [])     -> "void*"
        ("[]",   [a])     -> showProxList a ++ "*"
        ("(,)",  [_, _]) -> "Pair*"
        ("->",   [a, b]) -> showProxList b ++ " (*)(" ++ showProxList a ++ ")"
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
showCExpression (Var i) = "v" ++ show i
showCExpression (Not x) = "!" ++ showCExpression x
showCExpression (LIntOp op x y) = "(" ++ showCExpression x ++ " " ++ showBinOp op ++ " " ++ showCExpression y ++ ")"
showCExpression (LCmpOp op x y) = "(" ++ showCExpression x ++ " " ++ showCmpOp op ++ " " ++ showCExpression y ++ ")"
showCExpression (Val v) = showCValue v
showCExpression (CallExpr f arg) = showCExpression f ++ "(" ++ showCExpression arg ++ ")"
showCExpression (Prod l r) = "(" ++ showCExpression l ++ "," ++ showCExpression r ++ ")"
showCExpression (Fst p) = showCExpression p ++ "[0]"
showCExpression (Snd p) = showCExpression p ++ "[1]"
showCExpression EmptyList = "NULL"
showCExpression (ConsList x l) = "cons(&(" ++ showProx (DType.typeRep x) ++ "){" ++ showCExpression x ++ "}, " ++ showCExpression l ++ ")"
showCExpression (IsEmpty l) = "isEmpty(" ++ showCExpression l ++ ")"
showCExpression (HeadList l) = "*(" ++ showProxList (DType.typeRep l) ++ ")" ++ "head(" ++ showCExpression l ++ ")"
showCExpression (TailList l) = "tail(" ++ showCExpression l ++ ")"
showCExpression (IndexList l i) = showCExpression l ++ "[" ++ showCExpression i ++ "]"
showCExpression (Ternary cond thn els) = "(" ++ showCExpression cond ++ ") ? (" ++ showCExpression thn ++ ") : (" ++ showCExpression els ++ ")"

showEnv :: Env -> String
showEnv Empty = ""
showEnv (Extend i x r) = "(" ++ show i ++ ": " ++ showCValue x ++ "), " ++ showEnv r

main :: IO ()
main = do
    let (nl, c') = NL.translate 0 AL.mapListCall
        cl = evalState (translate nl) c'
        ev = eval cl Empty
    putStrLn $ NL.pretty nl
    putStrLn "--- Translating ---"
    putStrLn $ showCStmt 0 cl
    -- putStrLn $ showLitStmt 0 cl
    putStrLn "\n--- Evaluating ---"
    putStrLn $ showCValue ev