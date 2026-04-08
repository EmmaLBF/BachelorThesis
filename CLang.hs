{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use first" -}

module CLang where
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map

import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import qualified NamedLang as NL

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    FuncV :: (CValue a -> CValue b) -> CValue (a -> b)

data CExpression a where
    Var :: Int -> CExpression a
    LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
    LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
    Val :: CValue a -> CExpression a

data CStatement a where
    Return :: CExpression a -> CStatement a
    Seq :: CStatement a -> (CValue a -> CStatement b) -> CStatement b
    If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a
    Def :: (CValue a -> CStatement b) -> CStatement (a -> b)
    Fix :: CStatement (a -> a) -> CStatement a
    While :: CExpression Bool -> CStatement a -> CStatement a -- condition + body
    Call :: CStatement (a -> b) -> CExpression a -> CStatement b

translateExpr :: NL.NamedLang a -> Map Int Dynamic -> CExpression a
translateExpr (NL.Var x) _ = Var x
translateExpr (NL.LInt x) _ = Val (IntV x)
translateExpr (NL.LBool x) _ = Val (BoolV x)
translateExpr (NL.LIntOp x y z) m = LIntOp x (translateExpr y m) (translateExpr z m)
translateExpr (NL.LCmpOp x y z) m = LCmpOp x (translateExpr y m) (translateExpr z m)

translateStmt :: NL.NamedLang a -> CStatement a
translateStmt lang = trans lang Map.empty
    where
    trans :: NL.NamedLang a -> Map Int Dynamic -> CStatement a
    trans (NL.Lam i body) m = Def (\x -> trans body (Map.insert i (toDyn x) m))
    trans (NL.Fix f) m = Fix (trans f m)
    trans (NL.Apply x y) m = Call (trans x m) (translateExpr y m)
    trans (NL.If x y z) m = If (translateExpr x m) (trans y m) (trans z m)
    trans x m = Return (translateExpr x m)
-- translateStmt (NL.Prod x y) = ()
-- translateStmt (NL.Fst x) = ()
-- translateStmt (NL.Snd x) = ()


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
showCValue (FuncV _) = "<func>"

showCExpression :: CExpression a -> String
showCExpression (Var i) = "v" ++ show i
showCExpression (LIntOp op x y) = "(" ++ showCExpression x ++ " " ++ showBinOp op ++ " " ++ showCExpression y ++ ")"
showCExpression (LCmpOp op x y) = "(" ++ showCExpression x ++ " " ++ showCmpOp op ++ " " ++ showCExpression y ++ ")"
showCExpression (Val v) = showCValue v

showCStatement :: CStatement a -> String
showCStatement (Return e) = "return " ++ showCExpression e
showCStatement (Seq s _) = "seq (" ++ showCStatement s ++ ") <function>"
showCStatement (If c t e) = "if " ++ showCExpression c ++ " then " ++ showCStatement t ++ " else " ++ showCStatement e
showCStatement (Def _) = "def <function>"
showCStatement (Fix s) = "fix (" ++ showCStatement s ++ ")"
showCStatement (While c b) = "while " ++ showCExpression c ++ " do " ++ showCStatement b
showCStatement (Call f x) = showCStatement f ++ " (" ++ showCExpression x ++ ")"

main :: IO ()
main = do
  putStrLn "--- Translating FO ---"
  putStrLn $ "Factorial 5 = " ++ showCStatement (translateStmt (fst (NL.translate 0 AL.fac)))
 