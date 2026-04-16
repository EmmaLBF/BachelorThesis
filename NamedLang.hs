{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{- HLINT ignore "Use first" -}

module NamedLang where

import Data.Dynamic

import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import Data.Typeable

data NamedLang a where
  Var :: (Typeable a) => Int -> NamedLang a -- | Free variable
  Lam :: (Typeable a, Typeable b) => Proxy a -> Int -> NamedLang b -> NamedLang (a -> b) -- | Lambda abstraction
  Apply :: (Typeable a, Typeable b) => NamedLang (a -> b) -> NamedLang a -> NamedLang b -- | Function application
  Fix :: (Typeable a) => NamedLang (a -> a) -> NamedLang a -- | Recursion
  If :: (Typeable a) => NamedLang Bool -> NamedLang a -> NamedLang a -> NamedLang a -- | If-then-else
  -- | LITERALS
  LInt :: Int -> NamedLang Int  -- | Integer literal
  LBool :: Bool -> NamedLang Bool -- | Boolean literal
  -- | OPERATIONS
  LIntOp :: BinOp -> NamedLang Int -> NamedLang Int -> NamedLang Int -- | Binary integer operation
  LCmpOp :: CmpOp -> NamedLang Int -> NamedLang Int -> NamedLang Bool -- | Integer comparison
  -- | TUPLES
  Prod :: (Typeable a, Typeable b) => NamedLang a -> NamedLang b -> NamedLang (a, b)  -- | Make a tuple
  Fst :: (Typeable a, Typeable b) => NamedLang (a, b) -> NamedLang a -- | Project left
  Snd :: (Typeable a, Typeable b) => NamedLang (a, b) -> NamedLang b -- | Project right

translate :: Int -> AL.Lang a -> (NamedLang a, Int)
translate c (AL.Abs f) = let (body, c') = translate (c+1) (f (AL.Var c))
                         in (Lam (Proxy :: Proxy a) c body, c') -- (Lam c (fst (translate (c+1) (f (AL.Var c)))), c + 1)
translate c (AL.Var x) = (Var x, c)
translate c (AL.LInt x) = (LInt x, c)
translate c (AL.LBool x) = (LBool x, c)
translate c (AL.Fix x) = let (body, c') = translate c x
                         in (Fix body, c')
translate c (AL.Fst x) = let (body, c') = translate c x
                         in (Fst body, c')
translate c (AL.Snd x) = let (body, c') = translate c x
                         in (Snd body, c')
translate c (AL.Apply x y) = let p = translate c x
                                 q = translate (snd p) y
                             in (Apply (fst p) (fst q), snd q)
translate c (AL.If x y z) = let p = translate c x
                                q = translate (snd p) y
                                r = translate (snd q) z
                            in (If (fst p) (fst q) (fst r), snd r)
translate c (AL.LIntOp x y z) = let p = translate c y
                                    q = translate (snd p) z
                                in (LIntOp x (fst p) (fst q), snd q)
translate c (AL.LCmpOp x y z) = let p = translate c y
                                    q = translate (snd p) z
                                in (LCmpOp x (fst p) (fst q), snd q)
translate c (AL.Prod x y) = let p = translate c x
                                q = translate (snd p) y
                            in (Prod (fst p) (fst q), snd q)


pretty :: NamedLang a -> String
pretty expr = go [] expr
  where
    -- env maps variable index -> name
    go :: [(Int, String)] -> NamedLang t -> String
    go env e =
      case e of
        Var x ->
          case lookup x env of
            Just name -> name
            Nothing   -> "x" ++ show x

        Lam prox i f ->
          -- find next unused Int for naming
          let x = i
              name = "x" ++ show x
              env' = (x, name) : env
              body = f  -- **use the same Int** translate would have used
          in "(\\" ++ name ++ " ->\n\t " ++ go env' body ++ ")"

        Apply f a ->
          "(" ++ go env f ++ " " ++ go env a ++ ")"

        Fix f ->
          "(fix " ++ go env f ++ ")"

        If cond t el ->
          "(if " ++ go env cond
          ++ "\n\tthen " ++ go env t
          ++ "\n\telse " ++ go env el ++ ")"

        LInt n -> show n
        LBool b -> show b
        LIntOp op l r -> "(" ++ go env l ++ " " ++ show op ++ " " ++ go env r ++ ")"
        LCmpOp op l r -> "(" ++ go env l ++ " " ++ show op ++ " " ++ go env r ++ ")"
        Prod a b -> "(" ++ go env a ++ ", " ++ go env b ++ ")"
        Fst p -> "(fst " ++ go env p ++ ")"
        Snd p -> "(snd " ++ go env p ++ ")"

-- pick next free integer not used in env
nextFree :: [(Int, String)] -> Int
nextFree env = case env of
  [] -> 0
  xs -> 1 + maximum (map fst xs)


main :: IO ()
main = do
  putStrLn "--- Translating Examples ---"
  putStrLn $ "Factorial 5 = " ++ pretty (fst (translate 0 AL.fac))
