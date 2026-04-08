{-# LANGUAGE GADTs #-}
module IOPractice where

data IOAction a where 
    Return :: a -> IOAction a
    Input :: IOAction String 
    Print :: String -> IOAction ()
    Seq :: IOAction a -> (a -> IOAction b) -> IOAction b 
    SeqT :: IOAction a -> IOAction b -> IOAction b 

-- take home exercise: Make interpreter
eval :: IOAction a -> IO a
eval (Return x) = return x
eval (Seq x f) = do
    a <- eval x
    eval (f a)
eval (SeqT _ b) = eval b
eval Input = getLine
eval (Print s) = putStrLn s


example :: IOAction ()
example = Seq Input (\a -> Seq (Print a) (\x -> Print a) )
-- example2 = Seq Input (\a -> Input (\b -> Seq (Print a) (Print b)) )
-- example3 = Seq Input (\a -> let n = read a :: Int in loop n )
--     where loop n = if n == 0 then Return () else Seq (Print "Hello") (loop (n-1))


-- example3 = Input >>= \a -> let n = read a :: Int in loop n 
--     where loop n = if n == 0 then return () else Print "Hello" >> loop (n-1)

-- b = 3 
-- ( bla)

-- return 3 >>= (\b -> bla)

-- do b <- 3
--    bla

-- (a >> b) >> c == a >> (b >> c)


main :: IO () 
main = eval example