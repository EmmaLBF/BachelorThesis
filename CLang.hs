{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use first" -}

module CLang where
import Data.Dynamic
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
    Call :: CExpression (a -> b) -> CExpression a -> CStatement b

-- data CLangTerm a = CExpr (CExpression a) | CStmt (CStatement a)
-- data CExpr a where
--     FuncV :: (CExpr a -> CExpr b) -> CExpr (a -> b)
--     LInt :: Int -> CExpr Int
--     LBool :: Bool -> CExpr Bool
--     Var :: Int -> CExpr a
--     LIntOp :: AL.BinOp -> CExpr Int -> CExpr Int -> CExpr Int
--     LCmpOp :: AL.CmpOp -> CExpr a -> CExpr a -> CExpr Bool

-- data CStatement a where
--     Return :: CExpr a -> CStatement a
--     Seq :: CStatement a -> (CExpr a -> CStatement b) -> CStatement b
--     If :: CExpr Bool -> CStatement a -> CStatement a -> CStatement a
--     Def :: (CExpr a -> CStatement b) -> CStatement (a -> b)
--     Fix :: CStatement (a -> a) -> CStatement a
--     While :: CExpr Bool -> CStatement a -> CStatement a -- condition + body

translate :: Int -> NL.NamedLang a -> (CStatement a, Int)
translate c (NL.Lam i body) = let (body', c') = translate (c+1) body
                             in ( Def (\x -> body'), c')
translate c (NL.Fix f) = let (f', c') = translate c f
                         in ( Fix f', c')
translate c (NL.Apply x y) = let (x', c1) = translate c x
                                 (y', c2) = translate c1 y
                            in ( Call x' y', c2)
translate c (NL.Var x) = (Return (Var x), c)
translate c (NL.LInt x) = (Return (Val (IntV x)), c)
translate c (NL.LBool x) = (Return (Val (BoolV x)), c)
translate c (NL.If x y z) = let (p, c1) = translate c x
                                (q, c2) = translate c1 y
                                (r, c3) = translate c2 z
                            in (If p q r, c3)
translate c (NL.LIntOp x y z) = let (p, c1) = translate c y
                                    (q, c2) = translate c1 z
                                in (Return (LIntOp x p q), c2)
translate c (NL.LCmpOp x y z) = let (p, c1) = translate c y
                                    (q, c2) = translate c1 z
                                in (Return (LCmpOp x p q), c2)
translate c (NL.Prod x y) = ((), c)
translate c (NL.Fst x) = ((), c)
translate c (NL.Snd x) = ((), c)

-- toStatement :: CLangTerm a -> CStatement a
-- toStatement (CExpr x) = Return x
-- toStatement (CStmt x) = x

main :: IO ()
main = do
  putStrLn "--- Translating FO ---"
  putStrLn $ "Factorial 5 = " ++ show (fst (translate 0 (fst (NL.translate 0 AL.fac))))