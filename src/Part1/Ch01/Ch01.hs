-- | 1. Introduction
module Part1.Ch01.Ch01 where

import Prelude hiding (sum)

-- | 1.1 Functions
-- >>> double 3
-- 6
double :: Num a => a -> a
double x = x + x

-- | 1.5
-- Summing
-- >>> sum [1,2,3]
-- 6
sum :: Num p => [p] -> p
sum [] = 0
sum (n:ns) = n + sum ns

-- | Sorting values
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                   smaller = [a | a <- xs, a <= x]
                   larger  = [b | b <- xs, b > x]

-- | Sequencing actions
seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)
