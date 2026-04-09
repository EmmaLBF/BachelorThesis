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

indentStr :: Int -> String
indentStr n = replicate (2 * n) ' '

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    -- FuncV :: (CValue a -> CValue b) -> CValue (a -> b)

data CExpression a where
    Var :: (Typeable a) => Int -> CExpression a
    LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
    LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
    Val :: CValue a -> CExpression a
    CallExpr :: CStatement (b -> a) -> CExpression b -> CExpression a

data CStatement a where
    Return :: CExpression a -> CStatement a
    Seq :: CStatement a -> (CValue a -> CStatement b) -> CStatement b
    If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a
    Def :: (Typeable a) => Int -> (CValue a -> Either String (CStatement b)) -> CStatement (a -> b)
    Fix :: (CValue a -> Either String (CStatement a)) -> CStatement a
    While :: CExpression Bool -> CStatement a -> CStatement a -- condition + body
    Call :: CStatement (a -> b) -> CExpression a -> CStatement b

translateExpr :: NL.NamedLang a -> Map Int Dynamic -> Either String (CExpression a)
translateExpr (NL.Var x) _ = Right (Var x)
translateExpr (NL.LInt x) _ = Right (Val (IntV x))
translateExpr (NL.LBool x) _ = Right (Val (BoolV x))
translateExpr (NL.LIntOp x y z) m = do p <- translateExpr y m
                                       q <- translateExpr z m
                                       return (LIntOp x p q)
translateExpr (NL.LCmpOp x y z) m = do p <- translateExpr y m
                                       q <- translateExpr z m
                                       return (LCmpOp x p q)
translateExpr (NL.Apply x y) m = do func <- trans x m
                                    arg <- translateExpr y m
                                    return (CallExpr func arg)
translateExpr _ _ = Left "expected expression, got statement"

trans :: NL.NamedLang a -> Map Int Dynamic -> Either String (CStatement a)
trans (NL.Lam i body) m = Right (Def i (\x -> let m' = Map.insert i (toDyn x) m
                                            in trans body m'))
trans (NL.Fix (NL.Lam i body)) m = Right (Fix (\x -> let m' = Map.insert i (toDyn x) m
                                                        in trans body m'))
trans (NL.Apply x y) m = do x' <- trans x m
                            y' <- translateExpr y m
                            return (Call x' y')
trans (NL.If x y z) m = do p <- translateExpr x m
                           q <- trans y m
                           r <- trans z m
                           return (If p q r)                        
trans x m = do p <- translateExpr x m
               return (Return p)

translateStmt :: NL.NamedLang a -> Either String (CStatement a)
translateStmt lang = trans lang Map.empty
    
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
                            Just v -> (v,  m)
                            Nothing -> error "Type mismatch in env"
                        Nothing -> error "Variable not found"
evalExpr (LIntOp op lhs rhs) m = let (lhs', m') = evalExpr lhs m
                                     (rhs', m'') = evalExpr rhs m'
                                    in (IntV (AL.binop op (valueToLiteral lhs') (valueToLiteral rhs')), m'')
evalExpr (LCmpOp op lhs rhs) m = let (lhs', m') = evalExpr lhs m
                                     (rhs', m'') = evalExpr rhs m'
                                    in (BoolV (AL.cmpop op (valueToLiteral lhs') (valueToLiteral rhs')), m'')
evalExpr (CallExpr f arg) m = let (fn, m') = evalFunc f m
                                  (arg', _) = evalExpr arg m'
                                  (res, m'') = fn arg'
                              in (res, m'')

eval :: CStatement a -> Map Int Dynamic -> (CValue a, Map Int Dynamic)
eval (Return x) m = evalExpr x m
eval (Seq first f) m = let (p, m') = eval first m
                        in eval (f p) m'
eval (If cond body alt) m = let (cond', m') = evalExpr cond m
                            in if valueToLiteral cond'
                               then eval body m'
                               else eval alt m'
eval (While cond body) m = let (cond', m') = evalExpr cond m
                           in if valueToLiteral cond' 
                              then let (_, m'') = eval body m'
                                   in eval (While cond body) m''
                              else error "While loop has no return value"
eval (Fix f) m = (fix (\x -> fst (eval (case f x of
                                        (Left e)  -> error e
                                        (Right v) -> v) m)), m)
eval (Call f arg) m = let (stmt, m') = evalFunc f m
                          (arg', _) = evalExpr arg m'
                          (v, m'') = stmt arg'
                      in (v, m'')
eval (Def _ _) _ = error "Cannot eval Def as a value — use evalFunc"

evalFunc :: CStatement (a -> b) -> Map Int Dynamic -> (CValue a -> (CValue b, Map Int Dynamic), Map Int Dynamic)
evalFunc (Def i body) m = (\x -> let m' = Map.insert i (toDyn x) m
                                  in case body x of
                                      Left e  -> error e
                                      Right v -> eval v m', m)
evalFunc (If cond t f) m = let (cond', m') = evalExpr cond m
                            in if valueToLiteral cond'
                               then evalFunc t m'
                               else evalFunc f m'
-- evalFunc (Call f arg) m = let (fn, m') = evalFunc f m
--                               (arg', _) = evalExpr arg m'
--                               (stmt, m'') = fn arg'
--                            in evalFunc stmt m''
evalFunc _ _ = error "Expected function-typed statement"

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

showCExpression :: Int -> CExpression a -> Map Int Dynamic -> String
showCExpression indent (Var i) _ = indentStr indent ++ "v" ++ show i
showCExpression indent (LIntOp op x y) m = indentStr indent ++ "(" ++ showCExpression indent x m ++ " " ++ showBinOp op ++ " " ++ showCExpression indent y m ++ ")"
showCExpression indent (LCmpOp op x y) m = indentStr indent ++ "(" ++ showCExpression indent x m ++ " " ++ showCmpOp op ++ " " ++ showCExpression indent y m ++ ")"
showCExpression indent (Val v) _ = indentStr indent ++ showCValue v
showCExpression indent (CallExpr f arg) m = indentStr indent ++ showStmt indent f m ++ "(" ++ showCExpression indent arg m ++ ")"

showStmt :: Int ->  CStatement a -> Map Int Dynamic -> String
showStmt indent (Def i f) m = case f (error "dummy") of
                        Left e  -> "\n" ++ indentStr indent ++ "Def " ++ show i ++ " -> Error: " ++ e
                        Right v -> "\n" ++ indentStr indent ++ "Def " ++ show i ++ " -> " ++ showStmt (indent + 1) v m
showStmt indent (Fix f) m   = case f (error "dummy") of
                        Left e  -> "\n" ++ indentStr indent ++ "Fix -> Error: " ++ e
                        Right v -> "\n" ++ indentStr indent ++ "Fix -> " ++ showStmt (indent + 1) v m
showStmt indent (Return x) m = indentStr indent ++ "[Return " ++ showCExpression indent x m ++ "]"
showStmt indent (Seq first f) m = indentStr indent ++ showStmt indent first m ++ "\n" ++ showStmt indent (f (fst (eval first m))) m
showStmt indent (If cond t f) m = "\n" ++ indentStr indent ++ "If " ++ showCExpression 0 cond m ++ " then \n" ++ indentStr (indent + 1) ++ showStmt 0 t m ++ " else \n" ++ indentStr (indent + 2) ++ showStmt 0 f m
showStmt indent (While cond body) m = "\n" ++ indentStr indent ++ "While " ++ showCExpression indent cond m ++ " do " ++ showStmt (indent + 1) body m
showStmt indent (Call f arg) m = "\n" ++ indentStr indent ++ showStmt indent f m ++ "(" ++ showCExpression indent arg m ++ ")"

main :: IO ()
main = do
  putStrLn "--- Translating FO ---"
  putStrLn $ "Factorial 5 = " ++ case translateStmt (fst (NL.translate 0 AL.fac)) of
    (Left err) -> show err
    (Right val) -> showStmt 0 val Map.empty
