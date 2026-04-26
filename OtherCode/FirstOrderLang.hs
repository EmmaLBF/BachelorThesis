{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use first" -}

module OtherCode.FirstOrderLang where

import qualified AbsLang as AL
import qualified NamedLang as NL

data FirstOrderExpr
  = Var Int
  | Lam Int FirstOrderExpr
  | App FirstOrderExpr FirstOrderExpr
  | Fix Int FirstOrderExpr
  | If FirstOrderExpr FirstOrderExpr FirstOrderExpr
  | LInt Int
  | LBool Bool
  | LIntOp AL.BinOp FirstOrderExpr FirstOrderExpr
  | LCmpOp AL.CmpOp FirstOrderExpr FirstOrderExpr
  | Prod FirstOrderExpr FirstOrderExpr
  | Fst FirstOrderExpr
  | Snd FirstOrderExpr
  deriving (Show)

-- translate :: Int -> NL.NamedLang a -> (FirstOrderExpr, Int)
-- translate c (NL.Lam i f) = let (body, c') = translate (c + 1) (f c)
--                          in (Lam c body, c')
-- translate c (NL.Var x) = (Var x, c)
-- translate c (NL.LInt x) = (LInt x, c)
-- translate c (NL.LBool x) = (LBool x, c)
-- translate c (NL.Fix f) = let (body, c') = translate c f
--                          in (Fix c' body, c')
-- translate c (NL.Fst x) = let (body, c') = translate c x
--                          in (Fst body, c')
-- translate c (NL.Snd x) = let (body, c') = translate c x
--                          in (Snd body, c')
-- translate c (NL.Apply x y) = let (body1, c'1) = translate c x
--                                  (body2, c'2) = translate c'1 y
--                              in (App body1 body2, c'2)
-- translate c (NL.If x y z) = let p = translate c x
--                                 q = translate (snd p) y
--                                 r = translate (snd q) z
--                             in (If (fst p) (fst q) (fst r), snd r)
-- translate c (NL.LIntOp x y z) = let p = translate c y
--                                     q = translate (snd p) z
--                                 in (LIntOp x (fst p) (fst q), snd q)
-- translate c (NL.LCmpOp x y z) = let p = translate c y
--                                     q = translate (snd p) z
--                                 in (LCmpOp x (fst p) (fst q), snd q)
-- translate c (NL.Prod x y) = let p = translate c x
--                                 q = translate (snd p) y
--                             in (Prod (fst p) (fst q), snd q)

-- main :: IO ()
-- main = do
--   putStrLn "--- Translating FO ---"
--   putStrLn $ "Factorial 5 = " ++ show (fst (translate 0 (fst (NL.translate 0 AL.fac))))