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

data BinOp = Plus | Min | Times | Div | Mod deriving Show

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
      ev (fresh + 1) (Map.insert fresh (toDyn v) env) (f (Var fresh))
    ev _ _ (LBool b) = b
    ev _ _ (LInt i) = i
    ev _ _ EmptyList = []
    ev fresh env (If c t e) =
      if ev fresh env c then ev fresh env t else ev fresh env e
    ev fresh env (Not x) = not (ev fresh env x)
    ev fresh env (Abs x) = abs (ev fresh env x)
    ev fresh env (Apply f x) = ev fresh env f (ev fresh env x)
    ev fresh env (LIntOp op l r) = binop op (ev fresh env l) (ev fresh env r)
    ev fresh env (LBoolOp op l r) = boolop op (ev fresh env l) (ev fresh env r)
    ev fresh env (LCmpOp op l r) = cmpop op (ev fresh env l) (ev fresh env r)
    ev fresh env (Prod l r) = (ev fresh env l, ev fresh env r)
    ev fresh env (Fst p) = fst (ev fresh env p)
    ev fresh env (Snd p) = snd (ev fresh env p)
    ev fresh env (Fix f) = fix (ev fresh env f)
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

-- 1. Factorial
fac :: Lang (Int -> Int)
fac = Fix $ lam $ \f -> lam $ \n ->
  If (n ==: int 0)
    (int 1)
    (n *: (f `app` (n -: int 1)))

facCall :: Lang Int
facCall = fac `app` int 5

-- 2. Fibonacci
fib :: Lang (Int -> Int)
fib = Fix $ lam $ \f -> lam $ \n ->
  If (n <: int 2)
    n
    ((f `app` (n -: int 1)) +: (f `app` (n -: int 2)))

fibCall :: Lang Int
fibCall = fib `app` int 5

-- 3. GCD (using tuples to pass two arguments recursively)
gcdLang :: Lang ((Int, Int) -> Int)
gcdLang = Fix $ lam $ \f -> lam $ \p ->
  let_ (Fst p) $ \a ->
    let_ (Snd p) $ \b ->
      If (b ==: int 0)
        a
        (f `app` Prod b (a %: b))

gcdLangCall :: Lang Int
gcdLangCall = gcdLang `app` Prod (int 30) (int 10)

-- 4. Power Function
power :: Lang ((Int, Int) -> Int)
power = Fix $ lam $ \f -> lam $ \p ->
  let_ (Fst p) $ \b ->
    let_ (Snd p) $ \e ->
      If (e ==: int 0)
        (int 1)
        (b *: (f `app` Prod b (e -: int 1)))

powerCall :: Lang Int
powerCall = power `app` Prod (int 4) (int 2)

-- 5. Collatz Conjecture (count steps to reach 1)
collatzSteps :: Lang (Int -> Int)
collatzSteps = Fix $ lam $ \f -> lam $ \n ->
  If (n ==: int 1)
    (int 0)
    ( int 1
        +: ( f `app` If ((n %: int 2) ==: int 0)
                 (n /: int 2)
                 ((int 3 *: n) +: int 1)
           )
    )

collatzCall :: Lang Int
collatzCall = collatzSteps `app` int 8

-- 6. Efficient Fibonacci (O(n) using tuples as state)
-- fibFastHelper computes (a, b) after n steps
fibFastHelper :: Lang ((Int, (Int, Int)) -> (Int, Int))
fibFastHelper = Fix $ lam $ \f -> lam $ \p ->
  let_ (Fst p) $ \n ->
    let_ (Snd p) $ \state ->
      let_ (Fst state) $ \a ->
        let_ (Snd state) $ \b ->
          If (n ==: int 0)
          state
          (f `app` Prod (n -: int 1) (Prod b (a +: b)))

-- Start with state (0, 1) and return the first element of the result
fibFast :: Lang (Int -> Int)
fibFast = lam $ \n ->
  Fst (fibFastHelper `app` Prod n (Prod (int 0) (int 1)))

fibFastCall :: Lang Int
fibFastCall = fibFast `app` int 6

-- 7. Ackermann Function (Deep Recursion Test)
ackermann :: Lang ((Int, Int) -> Int)
ackermann = Fix $ lam $ \f -> lam $ \p ->
  let_ (Fst p) $ \m ->
    let_ (Snd p) $ \n ->
      If (m ==: int 0)
        (n +: int 1)
        ( If
            (n ==: int 0)
            (f `app` Prod (m -: int 1) (int 1))
            (f `app` Prod (m -: int 1) (f `app` Prod m (n -: int 1)))
        )
  
ackermannCall :: Lang Int
ackermannCall = ackermann `app` Prod (int 2) (int 6)

-- 8. Sum List
sumList :: Lang ([Int] -> Int)
sumList = Fix $ lam $ \f -> lam $ \xs ->
  CaseList xs
    (int 0)
    (lam $ \h -> lam $ \t -> h +: (f `app` t))

sumListCall :: Lang Int
sumListCall = sumList `app` (int 1 `cons` (int 2 `cons` (int 3 `cons` nil)))

-- 9. length of a list
lenList :: Typeable a => Lang ([a] -> Int)
lenList = Fix $ lam $ \f -> lam $ \xs ->
  CaseList xs
    (int 0)
    (lam $ \_ -> lam $ \t -> int 1 +: (f `app` t))

lenListCall :: Lang Int
lenListCall = lenList `app` (int 1 `cons` (int 2 `cons` (int 3 `cons` nil)))

-- 10. map over a list
mapList :: Lang ((Int -> Int) -> [Int] -> [Int])
mapList = Fix $ lam $ \f -> lam $ \g -> lam $ \xs ->
  CaseList xs
    EmptyList
    (lam $ \h -> lam $ \t -> ConsList (g `app` h) (f `app` g `app` t))

mapListCall :: Lang [Int]
mapListCall = (mapList `app` (lam $ \x -> x *: int 2))
                      `app` (int 1 `cons` (int 2 `cons` (int 3 `cons` nil)))

-- 11. mergesort
mergeList :: Lang ([Int] -> [Int] -> [Int])
mergeList = Fix $ lam $ \f -> lam $ \first -> lam $ \second ->
  CaseList first
    second
    (lam $ \hFirst -> lam $ \tFirst ->
      CaseList second
        first
        (lam $ \hSecond -> lam $ \tSecond ->
          If (hFirst <: hSecond)
            (cons hFirst ((f `app` tFirst) `app` second))
            (cons hSecond ((f `app` tSecond) `app` first))
        )
      )

splitN :: Lang ((Int, [Int]) -> ([Int], [Int]))
splitN = Fix $ lam $ \f -> lam $ \p ->
  let_ (Fst p) $ \n ->
    let_ (Snd p) $ \xs ->
      If (n ==: int 0)
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
            let_ (splitHalf `app` cons h (cons th tt)) $ \p ->
              (mergeList `app` (f `app` Fst p)) `app` (f `app` Snd p)))

mergeSortCall :: Lang [Int]
mergeSortCall = mergeSort `app` (int 4 `cons` (int 6 `cons` (int 3 `cons` nil)))

-- 12. N-queens

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

nQueens :: Lang (Int -> [[(Int, Int)]])
nQueens =
  lam $ \n ->
    (Fix $ lam $ \f ->
      lam $ \row ->
        lam $ \partials ->
          If (row ==: n)
            partials
            (f `app` (row +: int 1)
               `app` (extendAll `app` n `app` row `app` partials)))
    `app` int 0 `app` cons nil nil

nQueensCall :: Lang Int
nQueensCall = lenList `app` (nQueens `app` int 4)

-- alternative version of nQueens with a lambda as an argument
nQueens1 :: Lang (Int -> [[(Int, Int)]])
nQueens1 = 
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

nQueensCall1 :: Lang Int
nQueensCall1 = lenList `app` (nQueens1 `app` int 4)
