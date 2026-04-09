{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use first" -}

module CLang where
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Fix

import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import qualified NamedLang as NL

data CFunc a where
    Func :: (CValue a -> Either String (CStatement b)) -> CFunc (a -> b)

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    -- FuncV :: (CValue a -> CValue b) -> CValue (a -> b)

data CExpression a where
    Var :: Int -> CExpression a
    LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
    LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
    Val :: CValue a -> CExpression a

data CStatement a where
    Return :: CExpression a -> CStatement a
    Seq :: CStatement a -> (CValue a -> CStatement b) -> CStatement b
    If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a
    Def :: CFunc (a -> b) -> CStatement (a -> b)
    Fix :: CFunc (a -> a) -> CStatement a
    While :: CExpression Bool -> CStatement a -> CStatement a -- condition + body
    Call :: CFunc (a -> b) -> CExpression a -> CStatement b

translateExpr :: NL.NamedLang a -> Either String (CExpression a)
translateExpr (NL.Var x) = Right (Var x)
translateExpr (NL.LInt x) = Right (Val (IntV x))
translateExpr (NL.LBool x) = Right (Val (BoolV x))
translateExpr (NL.LIntOp x y z) = do p <- translateExpr y
                                     q <- translateExpr z
                                     return (LIntOp x p q)
translateExpr (NL.LCmpOp x y z) = do p <- translateExpr y
                                     q <- translateExpr z
                                     return (LCmpOp x p q)
translateExpr _ = Left "expected expression, got statement"

translateStmt :: NL.NamedLang a -> Either String (CStatement a)
translateStmt lang = trans lang Map.empty
    where
    trans :: NL.NamedLang a -> Map Int Dynamic -> Either String (CStatement a)
    trans (NL.Lam i body) m = Right (Def (Func (\x -> let m' = Map.insert i (toDyn x) m
                                                in trans body m')))
    trans (NL.Fix (NL.Lam i body)) m = Right (Fix (Func (\x -> let m' = Map.insert i (toDyn x) m
                                                          in trans body m')))
    trans (NL.Apply x y) m = do x' <- trans x m
                                y' <- translateExpr y
                                return (Call (Func x') y')
    trans (NL.If x y z) m = do p <- translateExpr x
                               q <- trans y m
                               r <- trans z m
                               return (If p q r)
    trans x _ = do p <- translateExpr x
                   return (Return p)
-- translateStmt (NL.Prod x y) = ()
-- translateStmt (NL.Fst x) = ()
-- translateStmt (NL.Snd x) = ()

valueToLiteral :: CValue a -> a
valueToLiteral (IntV i) = i
valueToLiteral (BoolV i) = i

evalExpr :: CExpression a -> Map Int Dynamic -> (CValue a, Map Int Dynamic)
evalExpr (Val x) m = (x, m)
evalExpr (Var i) m = case Map.lookup i m of
                        Just dyn -> case fromDynamic dyn of
                            Just v -> (v, m)
                            Nothing -> error "Type mismatch in env"
                        Nothing -> error "Variable not found"
evalExpr (LIntOp op lhs rhs) m = let (lhs', m1) = evalExpr lhs m
                                     (rhs', m2) = evalExpr rhs m1
                                    in (IntV (AL.binop op (valueToLiteral lhs') (valueToLiteral rhs')), m2)
evalExpr (LCmpOp op lhs rhs) m = let (lhs', m1) = evalExpr lhs m
                                     (rhs', m2) = evalExpr rhs m1
                                    in (BoolV (AL.cmpop op (valueToLiteral lhs') (valueToLiteral rhs')), m2)

evalFunc :: CStatement (a -> b) -> Map Int Dynamic -> Either String (CFunc (a -> b))
evalFunc (Def f) m = (\x -> fst (eval (case body x of
                                        (Left e) -> error e
                                        (Right v) -> v) m), m)
evalFunc (Fix f) m = ()
evalFunc (Call f arg) m = ()
evalFunc _ _ = ()

eval :: CStatement a -> Map Int Dynamic -> (CValue a, Map Int Dynamic)
eval (Return x) m = evalExpr x m
eval (Seq first f) m = let (p, m') = eval first m
                        in eval (f p) m'
eval (If cond body alt) m = let (cond', m1) = evalExpr cond m
                            in if valueToLiteral cond' then eval body m1 else eval alt m1
eval (Def body) m = (\x -> fst (eval (case body x of
                                        (Left e) -> error e
                                        (Right v) -> v) m), m)
eval (Fix f) m = (fix (\x -> fst (eval (case f x of
                                                (Left e) -> error e
                                                (Right v) -> v) m)), m)
eval (While cond body) m = let (cond', m1) = evalExpr cond m
                            in if valueToLiteral cond' then eval (While cond body) m1 else 0 m1
eval (Call f arg) m = let (arg', m') = evalExpr arg m
                          (fn, m2) = eval f m'
                        in (fn arg', m2)

class ToValue a where
    toValue :: a -> CValue a

instance ToValue Int where
    toValue = IntV

instance ToValue Bool where
    toValue = BoolV

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
-- showCValue (FuncV _) = "<func>"

showCExpression :: CExpression a -> String
showCExpression (Var i) = "v" ++ show i
showCExpression (LIntOp op x y) = "(" ++ showCExpression x ++ " " ++ showBinOp op ++ " " ++ showCExpression y ++ ")"
showCExpression (LCmpOp op x y) = "(" ++ showCExpression x ++ " " ++ showCmpOp op ++ " " ++ showCExpression y ++ ")"
showCExpression (Val v) = showCValue v

showCStatement :: CStatement a -> String
showCStatement (Return e) = "return " ++ showCExpression e
showCStatement (Seq s _) = "seq (" ++ showCStatement s ++ ") <function>"
showCStatement (If c t e) = "if " ++ showCExpression c ++ " then " ++ showCStatement t ++ " else " ++ showCStatement e
showCStatement (Def f) = "hello"
showCStatement (Fix s) = "fix (" ++ showCStatement s ++ ")"
showCStatement (While c b) = "while " ++ showCExpression c ++ " do " ++ showCStatement b
showCStatement (Call f x) = showCStatement f ++ " (" ++ showCExpression x ++ ")"

main :: IO ()
main = do
  putStrLn "--- Translating FO ---"
  putStrLn $ "Factorial 5 = " ++ case translateStmt (fst (NL.translate 0 AL.exArith)) of
    (Left err) -> show err
    (Right val) -> showCStatement val
