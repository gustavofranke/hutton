module Part1.Ch01.Ch01 where

-- 1. Introduction
-- 1.1 Functions
double :: Num a => a -> a
double x = x + x

a :: Integer
a = double 3

-- 1.5
-- Summing
sum' :: Num p => [p] -> p
sum' [] = 0
sum' (n:ns) = n + sum' ns

b :: Integer
b = sum' [1,2,3]

-- Sorting values
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                   smaller = [a | a <- xs, a <= x]
                   larger  = [b | b <- xs, b > x]

-- Sequencing actions
seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)
