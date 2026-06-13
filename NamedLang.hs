{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NamedLang where
import Data.Dynamic
import AbsLang (BinOp(..), CmpOp(..), BoolOp(..))
import qualified AbsLang as AL
import Data.Typeable
import Control.Monad.State

fresh :: State Int Int
fresh = do
  n <- get
  modify (+1)
  return n

data NamedLang a where
  Var :: (Typeable a) => Int -> NamedLang a
  Lam :: (Typeable a, Typeable b) => Proxy a -> Int -> NamedLang b -> NamedLang (a -> b)
  Apply :: (Typeable a, Typeable b) => NamedLang (a -> b) -> NamedLang a -> NamedLang b
  Fix :: (Typeable a) => NamedLang (a -> a) -> NamedLang a 
  If :: (Typeable a) => NamedLang Bool -> NamedLang a -> NamedLang a -> NamedLang a
  LInt :: Int -> NamedLang Int
  LBool :: Bool -> NamedLang Bool
  LIntOp :: BinOp -> NamedLang Int -> NamedLang Int -> NamedLang Int
  LCmpOp :: CmpOp -> NamedLang Int -> NamedLang Int -> NamedLang Bool
  LBoolOp :: BoolOp -> NamedLang Bool -> NamedLang Bool -> NamedLang Bool
  Not :: NamedLang Bool -> NamedLang Bool
  Abs :: NamedLang Int -> NamedLang Int
  Prod :: (Typeable a, Typeable b) => NamedLang a -> NamedLang b -> NamedLang (a, b)
  Fst :: (Typeable a, Typeable b) => NamedLang (a, b) -> NamedLang a
  Snd :: (Typeable a, Typeable b) => NamedLang (a, b) -> NamedLang b
  EmptyList  :: Typeable a => NamedLang [a]
  ConsList :: Typeable a => NamedLang a -> NamedLang [a] -> NamedLang [a]
  CaseList :: (Typeable a, Typeable b) => NamedLang [a] -> NamedLang b -> NamedLang (a -> [a] -> b) -> NamedLang b

translate :: AL.Lang a -> State Int (NamedLang a)
translate (AL.Lam f) = do
  c <- fresh
  Lam (Proxy :: Proxy a) c <$> translate (f (AL.Var c))
translate (AL.Var x) = return (Var x)
translate (AL.LInt x) = return (LInt x)
translate (AL.LBool x) = return (LBool x)
translate AL.EmptyList = return EmptyList
translate (AL.Fix x) = Fix <$> translate x
translate (AL.Fst x) = Fst <$> translate x
translate (AL.Snd x) = Snd <$> translate x
translate (AL.Not x) = Not <$> translate x
translate (AL.Abs x) = Abs <$> translate x
translate (AL.Prod x y) = Prod <$> translate x <*> translate y
translate (AL.Apply x y) = Apply <$> translate x <*> translate y
translate (AL.ConsList x y) = ConsList <$> translate x <*> translate y
translate (AL.LCmpOp op x y) = LCmpOp op <$> translate x <*> translate y
translate (AL.LIntOp op x y) = LIntOp op <$> translate x <*> translate y
translate (AL.LBoolOp op x y) = LBoolOp op <$> translate x <*> translate y
translate (AL.If x y z) = If <$> translate x <*> translate y <*> translate z
translate (AL.CaseList x y z) = CaseList <$> translate x <*> translate y <*> translate z

instance Show (NamedLang a) where
  show e = 
    case e of
      Var x -> "x" ++ show x
      Lam _ i f -> "(\\x" ++ show i ++ " ->\n\t " ++ show f ++ ")"
      If cond t el -> "(if " ++ show cond ++ "\n\tthen " ++ show t ++ "\n\telse " ++ show el ++ ")"
      Apply f a -> "(" ++ show f ++ " " ++ show a ++ ")"
      Fix f -> "(fix " ++ show f ++ ")"
      LInt n -> show n
      LBool b -> show b
      LIntOp op l r -> "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
      LCmpOp op l r -> "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
      LBoolOp op l r -> "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
      Not p -> "(! " ++ show p ++ ")"
      Abs p -> "|" ++ show p ++ "|"
      Prod a b -> "(" ++ show a ++ ", " ++ show b ++ ")"
      Fst p -> "(fst " ++ show p ++ ")"
      Snd p -> "(snd " ++ show p ++ ")"
      EmptyList -> "[]"
      ConsList x l -> show x ++ ":" ++ show l
      CaseList l nilCase consCase -> "case " ++ show l ++ " of\n  [] -> " ++ show nilCase ++ "\n  (h:t) ->" ++ show consCase
