{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AbsLang where

import Control.Monad.Fix
import Data.Dynamic ( toDyn, Typeable, Dynamic, fromDynamic )
import Data.Map (Map)
import qualified Data.Map as Map

data BinOp = Plus | Min | Times | Div | Mod
  deriving Show

data CmpOp = Eq | Lt | Gt
  deriving Show

-- ─────────────────────────────────────────────
--  The Language (GADT, typed via phantom param)
-- ─────────────────────────────────────────────

data Lang a where
  -- | Free variable – looked up in the environment at eval time
  Var :: (Typeable a) => Int -> Lang a
  -- | Lambda abstraction, using Higher order abstract syntax
  Abs :: (Typeable a, Typeable b) => (Lang a -> Lang b) -> Lang (a -> b)
  -- | Function application
  Apply :: (Typeable a, Typeable b) => Lang (a -> b) -> Lang a -> Lang b
  -- | Add recursion (for mutual recursion combine with tuples)
  Fix :: (Typeable a) => Lang (a -> a) -> Lang a
  If :: (Typeable a) => Lang Bool -> Lang a -> Lang a -> Lang a
  -- | LITERALS
  LInt :: Int -> Lang Int
  LBool :: Bool -> Lang Bool
  -- | OPERATIONS
  LIntOp :: BinOp -> Lang Int -> Lang Int -> Lang Int
  LCmpOp :: CmpOp -> Lang Int -> Lang Int -> Lang Bool
  -- | TUPLES
  Prod :: (Typeable a, Typeable b) => Lang a -> Lang b -> Lang (a, b)
  Fst :: (Typeable a, Typeable b) => Lang (a, b) -> Lang a
  Snd :: (Typeable a, Typeable b) => Lang (a, b) -> Lang b

binop :: BinOp -> Int -> Int -> Int
binop Min = (-)
binop Plus = (+)
binop Div = div
binop Times = (*)
binop Mod = mod

cmpop :: CmpOp -> Int -> Int -> Bool
cmpop Lt = (<)
cmpop Eq = (==)
cmpop Gt = (>)

eval :: Lang a -> a
eval = ev 0 Map.empty
  where
    ev :: Int -> Map Int Dynamic -> Lang a -> a
    ev _ env (Var k) =
      case Map.lookup k env of
        Just dyn -> case fromDynamic dyn of
          Just v -> v
          Nothing -> error "Type mismatch in env"
        Nothing -> error "Variable not found"
    ev fresh env (Abs f) = \v ->
      let env' = Map.insert fresh (toDyn v) env
       in ev (fresh + 1) env' (f (Var fresh))
    ev fresh env (Apply f x) =
      let v = ev fresh env x
          fn = ev fresh env f
       in fn v
    ev _ _ (LBool b) = b
    ev _ _ (LInt i) = i
    ev fresh env (If c t e) =
      if ev fresh env c then ev fresh env t else ev fresh env e
    ev fresh env (LIntOp op l r) =
      let l' = ev fresh env l
          r' = ev fresh env r
       in binop op l' r'
    ev fresh env (LCmpOp op l r) =
      let l' = ev fresh env l
          r' = ev fresh env r
       in cmpop op l' r'
    ev fresh env (Prod l r) = (ev fresh env l, ev fresh env r)
    ev fresh env (Fst p) = fst (ev fresh env p)
    ev fresh env (Snd p) = snd (ev fresh env p)
    ev fresh env (Fix f) = fix (ev fresh env f)

-- Syntactic Sugar
lam :: (Typeable a, Typeable b) => (Lang a -> Lang b) -> Lang (a -> b)
lam = Abs

app :: (Typeable a, Typeable b) => Lang (a -> b) -> Lang a -> Lang b
app = Apply

-- | Syntactic sugar for Let bindings `let x = v in e`.
-- In lambda calculus this is mathematically equivalent to `(\x -> e) v`.
-- Thus, we can easily express local variables using only `Apply` and `Abs`!
let_ :: (Typeable a, Typeable b) => Lang a -> (Lang a -> Lang b) -> Lang b
let_ val body = app (lam body) val

(+:), (-:), (*:), (/:), (%:) :: Lang Int -> Lang Int -> Lang Int
a +: b = LIntOp Plus a b
a -: b = LIntOp Min a b
a *: b = LIntOp Times a b
a /: b = LIntOp Div a b
a %: b = LIntOp Mod a b

(==:), (<:), (>:) :: Lang Int -> Lang Int -> Lang Bool
a ==: b = LCmpOp Eq a b
a <: b = LCmpOp Lt a b
a >: b = LCmpOp Gt a b

int :: Int -> Lang Int
int = LInt

-- Examples
-- 1. Simple Arithmetic
exArith :: Lang Int
exArith = (int 10 +: int 2) *: int 3

-- 2. Factorial
fac :: Lang (Int -> Int)
fac = Fix $ lam $ \f -> lam $ \n ->
  If
    (n ==: int 0)
    (int 1)
    (n *: (f `app` (n -: int 1)))

facCall :: Lang Int
facCall =
  fac `app` int 5

-- 3. Fibonacci
fib :: Lang (Int -> Int)
fib = Fix $ lam $ \f -> lam $ \n ->
  If
    (n <: int 2)
    n
    ((f `app` (n -: int 1)) +: (f `app` (n -: int 2)))

-- 4. GCD (using tuples to pass two arguments recursively)
gcdLang :: Lang ((Int, Int) -> Int)
gcdLang = Fix $ lam $ \f -> lam $ \p ->
  let a = Fst p
      b = Snd p
   in If
        (b ==: int 0)
        a
        (f `app` Prod b (a %: b))

-- 5. Higher-Order Functions: apply a function twice
twice :: (Typeable a) => Lang ((a -> a) -> (a -> a))
twice = lam $ \f -> lam $ \x -> f `app` (f `app` x)

inc :: Lang (Int -> Int)
inc = lam $ \x -> x +: int 1

-- 6. Power Function
power :: Lang ((Int, Int) -> Int)
power = Fix $ lam $ \f -> lam $ \p ->
  let b = Fst p
      e = Snd p
   in If
        (e ==: int 0)
        (int 1)
        (b *: (f `app` Prod b (e -: int 1)))

-- 7. Collatz Conjecture (count steps to reach 1)
collatzSteps :: Lang (Int -> Int)
collatzSteps = Fix $ lam $ \f -> lam $ \n ->
  If
    (n ==: int 1)
    (int 0)
    ( int 1
        +: ( f
               `app` If
                 ((n %: int 2) ==: int 0)
                 (n /: int 2)
                 ((int 3 *: n) +: int 1)
           )
    )

-- 8. Efficient Fibonacci (O(n) using tuples as state)
-- fibFastHelper computes (a, b) after n steps
fibFastHelper :: Lang ((Int, (Int, Int)) -> (Int, Int))
fibFastHelper = Fix $ lam $ \f -> lam $ \p ->
  let n = Fst p
      state = Snd p
      a = Fst state
      b = Snd state
   in If
        (n ==: int 0)
        state
        (f `app` Prod (n -: int 1) (Prod b (a +: b)))

-- Start with state (0, 1) and return the first element of the result
fibFast :: Lang (Int -> Int)
fibFast = lam $ \n ->
  Fst (fibFastHelper `app` Prod n (Prod (int 0) (int 1)))

-- 9. Ackermann Function (Deep Recursion Test)
ackermann :: Lang ((Int, Int) -> Int)
ackermann = Fix $ lam $ \f -> lam $ \p ->
  let m = Fst p
      n = Snd p
   in If
        (m ==: int 0)
        (n +: int 1)
        ( If
            (n ==: int 0)
            (f `app` Prod (m -: int 1) (int 1))
            (f `app` Prod (m -: int 1) (f `app` Prod m (n -: int 1)))
        )

-- 10. Integer Square Root (Newton's Method using while-like recursion)
-- State is (n, guess). If nextGuess >= guess, we found the root.
-- Note: We use the encoded `let_` for nextX instead of a regular Haskell `let`.
-- A regular Haskell `let` just gives a name for a piece of the AST, which is then
-- duplicated when used.
-- By using the encoded `let_` (which compiles to lambda abstraction and application),
-- the expression is evaluated exactly once at runtime, and its resulting value is
-- bound in the environment and then reused.
isqrtHelper :: Lang ((Int, Int) -> Int)
isqrtHelper = Fix $ lam $ \f -> lam $ \p ->
  let n = Fst p
      x = Snd p
   in let_ ((x +: (n /: x)) /: int 2) $ \nextX ->
        If
          (nextX ==: x)
          x
          ( If
              (nextX >: x)
              x
              (f `app` Prod n nextX)
          )

isqrt :: Lang (Int -> Int)
isqrt = lam $ \n ->
  If
    (n ==: int 0)
    (int 0)
    (isqrtHelper `app` Prod n ((n /: int 2) +: int 1))

-- 11. Mutual Recursion using Tuples of Functions (isEven, isOdd)
evenOddPair :: Lang (Int -> Bool, Int -> Bool)
evenOddPair = Fix $ lam $ \f ->
  let isEvn = Fst f
      isOdd = Snd f
      isEvn' = lam $ \n ->
        If
          (n ==: int 0)
          (LBool True)
          (isOdd `app` (n -: int 1))
      isOdd' = lam $ \n ->
        If
          (n ==: int 0)
          (LBool False)
          (isEvn `app` (n -: int 1))
   in Prod isEvn' isOdd'

isEvenLang :: Lang (Int -> Bool)
isEvenLang = Fst evenOddPair

isOddLang :: Lang (Int -> Bool)
isOddLang = Snd evenOddPair

-- 12. Sum of Digits
sumDigits :: Lang (Int -> Int)
sumDigits = Fix $ lam $ \f -> lam $ \n ->
  If
    (n ==: int 0)
    (int 0)
    ((n %: int 10) +: (f `app` (n /: int 10)))

-- 13. Let Binding Example
-- Using our let_ syntactic sugar to bind x=42 and evaluate x + x
letExample :: Lang Int
letExample = let_ (int 42) $ \x ->
  x +: x

main :: IO ()
main = do
  putStrLn "--- Evaluating Examples ---"
  putStrLn $ "Let Example (let x = 42 in x + x) = " ++ show (eval letExample)
  putStrLn $ "Arith: (10 + 2) * 3 = " ++ show (eval exArith)
  putStrLn $ "Factorial 5 = " ++ show (eval (fac `app` int 5))
  putStrLn $ "Fibonacci 10 = " ++ show (eval (fib `app` int 10))
  let p = Prod (int 56) (int 42)
  putStrLn $ "GCD 56 42 = " ++ show (eval (gcdLang `app` p))
  putStrLn $ "Twice Inc 10 = " ++ show (eval (twice `app` inc `app` int 10))
  let p2 = Prod (int 2) (int 10)
  putStrLn $ "Power 2^10 = " ++ show (eval (power `app` p2))
  putStrLn $ "Collatz Steps for 27 = " ++ show (eval (collatzSteps `app` int 27))
  putStrLn $ "Fast Fibonacci 30 = " ++ show (eval (fibFast `app` int 30))
  putStrLn $ "Ackermann (3, 4) = " ++ show (eval (ackermann `app` Prod (int 3) (int 4)))
  putStrLn $ "Integer Sqrt 144 = " ++ show (eval (isqrt `app` int 144))
  putStrLn $ "Integer Sqrt 1000 = " ++ show (eval (isqrt `app` int 1000))
  putStrLn $ "Is 42 Even? = " ++ show (eval (isEvenLang `app` int 42))
  putStrLn $ "Is 42 Odd? = " ++ show (eval (isOddLang `app` int 42))
  putStrLn $ "Sum of digits of 12345 = " ++ show (eval (sumDigits `app` int 12345))