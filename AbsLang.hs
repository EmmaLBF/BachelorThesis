{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AbsLang where

import Control.Monad.Fix
import Data.Dynamic ( toDyn, Typeable, Dynamic, fromDynamic )
import Data.Map (Map)
import qualified Data.Map as Map

-- ─────────────────────────────────────────────
--  The Language (GADT, typed via phantom param)
-- ─────────────────────────────────────────────

data BinOp = Plus | Min | Times | Div | Mod
  deriving Show

data CmpOp = Eq | Lt | Gt
  deriving Show

data BoolOp = Or | And
  deriving Show

data Lang a where
  -- | Free variable – looked up in the environment at eval time
  Var :: (Typeable a) => Int -> Lang a
  -- | Lambda abstraction, using Higher order abstract syntax
  Lam :: (Typeable a, Typeable b) => (Lang a -> Lang b) -> Lang (a -> b)
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
  LBoolOp :: BoolOp -> Lang Bool -> Lang Bool -> Lang Bool
  Not :: Lang Bool -> Lang Bool
  Abs :: Lang Int -> Lang Int
  -- | TUPLES
  Prod :: (Typeable a, Typeable b) => Lang a -> Lang b -> Lang (a, b)
  Fst :: (Typeable a, Typeable b) => Lang (a, b) -> Lang a
  Snd :: (Typeable a, Typeable b) => Lang (a, b) -> Lang b
  -- | LISTS
  EmptyList  :: Typeable a => Lang [a]
  ConsList :: Typeable a => Lang a -> Lang [a] -> Lang [a]
  CaseList :: (Typeable a, Typeable b)
           => Lang [a]          -- list to match on
           -> Lang b            -- nil case
           -> Lang (a -> [a] -> b)  -- cons case: head -> tail -> result
           -> Lang b

-- ─────────────────────────────────────────────
--  Eval Functions
-- ─────────────────────────────────────────────

binop :: BinOp -> Int -> Int -> Int
binop Min = (-)
binop Plus = (+)
binop Div = div
binop Times = (*)
binop Mod = mod

boolop :: BoolOp -> Bool -> Bool -> Bool
boolop Or = (||)
boolop And = (&&)

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
    ev fresh env (Lam f) = \v ->
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
    ev fresh env (Not x) =
      let x' = ev fresh env x
       in not x'
    ev fresh env (Abs x) =
      let x' = ev fresh env x
       in abs x'
    ev fresh env (LBoolOp op l r) =
      let l' = ev fresh env l
          r' = ev fresh env r
       in boolop op l' r'
    ev fresh env (LCmpOp op l r) =
      let l' = ev fresh env l
          r' = ev fresh env r
       in cmpop op l' r'
    ev fresh env (Prod l r) = (ev fresh env l, ev fresh env r)
    ev fresh env (Fst p) = fst (ev fresh env p)
    ev fresh env (Snd p) = snd (ev fresh env p)
    ev fresh env (Fix f) = fix (ev fresh env f)
    ev _ _ EmptyList = []
    ev fresh env (ConsList x l) = ev fresh env x : ev fresh env l
    ev fresh env (CaseList l nilCase consCase) =
      case ev fresh env l of
        [] -> ev fresh env nilCase
        (h:t) -> ev fresh env consCase h t

-- ─────────────────────────────────────────────
--  Syntactic Sugar
-- ─────────────────────────────────────────────

lam :: (Typeable a, Typeable b) => (Lang a -> Lang b) -> Lang (a -> b)
lam = Lam

app :: (Typeable a, Typeable b) => Lang (a -> b) -> Lang a -> Lang b
app = Apply

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

(||:), (&&:) :: Lang Bool -> Lang Bool -> Lang Bool
a ||: b = LBoolOp Or a b
a &&: b = LBoolOp And a b

int :: Int -> Lang Int
int = LInt

bool :: Bool -> Lang Bool
bool = LBool

nil :: Typeable a => Lang [a]
nil = EmptyList

cons :: Typeable a => Lang a -> Lang [a] -> Lang [a]
cons = ConsList

-- ─────────────────────────────────────────────
--  Examples
-- ─────────────────────────────────────────────

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

fibCall :: Lang Int
fibCall =
  fib `app` int 5

-- 4. GCD (using tuples to pass two arguments recursively)
gcdLang :: Lang ((Int, Int) -> Int)
gcdLang = Fix $ lam $ \f -> lam $ \p ->
  let a = Fst p
      b = Snd p
   in If
        (b ==: int 0)
        a
        (f `app` Prod b (a %: b))

gcdLangCall :: Lang Int
gcdLangCall = gcdLang `app` Prod (int 30) (int 10)

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

-- 13. Sum List
sumList :: Lang ([Int] -> Int)
sumList = Fix $ lam $ \f -> lam $ \xs ->
  CaseList xs
    (int 0)
    (lam $ \h -> lam $ \t -> h +: (f `app` t))

sumListCall :: Lang Int
sumListCall = sumList `app` (int 1 `cons` (int 2 `cons` (int 3 `cons` nil)))

-- 14. length of a list
lenList :: Typeable a => Lang ([a] -> Int)
lenList = Fix $ lam $ \f -> lam $ \xs ->
  CaseList xs
    (int 0)
    (lam $ \_ -> lam $ \t -> int 1 +: (f `app` t))

lenListCall :: Lang Int
lenListCall = lenList `app` (int 1 `cons` (int 2 `cons` (int 3 `cons` nil)))

-- 15. map over a list
mapList :: Lang ((Int -> Int) -> [Int] -> [Int])
mapList = Fix $ lam $ \f -> lam $ \g -> lam $ \xs ->
  CaseList xs
    EmptyList
    (lam $ \h -> lam $ \t -> ConsList (g `app` h) (f `app` g `app` t))

mapListCall :: Lang [Int]
mapListCall = (mapList `app` (lam $ \x -> x *: int 2))
                      `app` (int 1 `cons` (int 2 `cons` (int 3 `cons` nil)))


-- 16. mergesort

mergeList :: Lang ([Int] -> [Int] -> [Int])
mergeList = Fix $ lam $ \f -> lam $ \first -> lam $ \second ->
  CaseList first
    second
    (lam $ \hFirst -> lam $ \tFirst ->
      CaseList second
        first
        (lam $ \hSecond -> lam $ \tSecond ->
          If
            (hFirst <: hSecond)
            (cons hFirst ((f `app` tFirst) `app` second))
            (cons hSecond ((f `app` tSecond) `app` first))
        )
      )

splitN :: Lang ((Int, [Int]) -> ([Int], [Int]))
splitN = Fix $ lam $ \f -> lam $ \p ->
  let_ (Fst p) $ \n ->
  let_ (Snd p) $ \xs ->
    If
      (n ==: int 0)
      (Prod nil xs)
      (CaseList xs
        (Prod nil nil)
        (lam $ \h -> lam $ \t ->
            let_ (f `app` Prod (n -: int 1) t) $ \recur->
              Prod
                (cons h (Fst recur))
                (Snd recur)
        )
      )

splitHalf :: Lang ([Int] -> ([Int], [Int]))
splitHalf = lam $ \xs ->
  let_ (lenList `app` xs) $ \n ->
  let_ (n /: int 2) $ \half ->
  splitN `app` Prod half xs

mergeSort :: Lang ([Int] -> [Int])
mergeSort = Fix $ lam $ \f -> lam $ \l ->
    CaseList l
      EmptyList
      (lam $ \h -> lam $ \t ->
        CaseList t
          (cons h EmptyList)
          (lam $ \th -> lam $ \tt ->
            let_ (splitHalf `app` (cons h (cons th tt))) $ \p ->
              (mergeList `app` (f `app` Fst p)) `app` (f `app` Snd p)))

mergeSortCall :: Lang [Int]
mergeSortCall = mergeSort `app` (int 4 `cons` (int 6 `cons` (int 3 `cons` nil)))

-- 17. N-queens

-- appendds two lists
appendList :: Typeable a => Lang ([a] -> [a] -> [a])
appendList =
  Fix $ lam $ \f ->
    lam $ \xs ->
      lam $ \ys ->
        CaseList xs
          ys
          (lam $ \h -> lam $ \t -> h `cons` (f `app` t `app` ys))

queensInterfere :: Lang ((Int, Int) -> (Int, Int) -> Bool)
queensInterfere =
  lam $ \q1 ->
    let_ (Fst q1) $ \r1 ->
    let_ (Snd q1) $ \c1 ->
      lam $ \q2 ->
        let_ (Fst q2) $ \r2 ->
        let_ (Snd q2) $ \c2 ->
          c1 ==: c2 ||: (Abs (c1 -: c2) ==: Abs (r1 -: r2))

queenSafe :: Lang ((Int, Int) -> [(Int, Int)] -> Bool)
queenSafe =
  Fix $ lam $ \f ->
    lam $ \newQ ->
      lam $ \placements ->
        CaseList placements
          (bool True)
          (lam $ \h ->
              lam $ \t ->
                Not (queensInterfere `app` newQ `app` h) &&: (f `app` newQ `app` t))

-- Try every column for `row`, extending `placed` with valid placements.
tryCols :: Lang (Int -> Int -> [(Int, Int)] -> Int -> [[(Int, Int)]])
tryCols = 
  Fix $ lam $ \f ->
    lam $ \n -> 
      lam $ \row -> 
        lam $ \placed -> 
          lam $ \col ->
            If (col ==: n)
              nil
              (let_ (Prod row col) $ \newQ ->
                let_ (f `app` n `app` row `app` placed `app` (col +: int 1)) $ \rest ->
                  If (queenSafe `app` newQ `app` placed)
                    (cons (cons newQ placed) rest)
                    rest)

-- Extend every partial solution by one row, concatenating.
extendAll :: Lang (Int -> Int -> [[(Int, Int)]] -> [[(Int, Int)]])
extendAll = 
  Fix $ lam $ \f ->
    lam $ \n -> 
      lam $ \row -> 
        lam $ \partials ->
          CaseList partials
            nil
            (lam $ \p -> 
              lam $ \ps ->
                (appendList `app` (tryCols `app` n `app` row `app` p `app` int 0))
                            `app` (f `app` n `app` row `app` ps))

-- Drive: extend row by row, starting from one empty partial.
nQueens :: Lang (Int -> [[(Int, Int)]])
nQueens = 
  lam $ \n ->
    let_ (Fix $ lam $ \f -> 
            lam $ \row -> 
              lam $ \partials ->
                If (row ==: n)
                  partials
                  (f `app` (row +: int 1)
                    `app` (extendAll `app` n `app` row `app` partials)))
        $ \solveRows ->
    solveRows `app` int 0 `app` cons nil nil

nQueensCall :: Lang Int
nQueensCall = lenList `app` (nQueens `app` int 4)

-- Test just extendAll without the solve loop
testExtendAll :: Lang Int
testExtendAll = lenList `app` (extendAll `app` int 4 `app` int 0 `app` (cons nil nil))

-- Test just tryCols directly
testTryCols :: Lang Int
testTryCols = lenList `app` ( tryCols `app` int 4 `app` int 0 `app` nil `app` int 0)

main :: IO ()
main = do
  putStrLn "--- Evaluating Examples ---"
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
  putStrLn $ "N-Queens " ++ show (eval (nQueens `app` int 4))