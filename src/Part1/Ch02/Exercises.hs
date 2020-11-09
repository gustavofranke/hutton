{-# LANGUAGE NoImplicitPrelude #-}

module Part1.Ch02.Exercises where

import GHC.Base
import GHC.List hiding (init, last)
import GHC.Num
import GHC.Real

-- 2 First steps
-- 2.7 Exercises
-- 1. Work through the examples from this chapter using GHCi.
-- DONE

-- 2.Parenthesise the following numeric expressions:

-- | 2^3*4
-- >>> b == 2^3*4
-- True
b :: Integer
b = (2 ^ 3) * 4

-- | 2*3+4*5
-- >>> c == 2*3+4*5
-- True
c :: Integer
c = (2 * 3) + (4 * 5)

-- | 2+3*4^5
-- d == 2+(3*(4^5))
-- True
d :: Integer
d = 2 + (3 * (4 ^ 5))

-- 3.The script below contains three syntactic errors.
-- Correct these errors and then check that your script works properly using GHCi.
-- N = a ’div’ length xs
-- where
-- a = 10
-- xs = [1,2,3,4,5]

-- |
-- >>> n
-- 2
n :: Int
n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

-- 4. The library function last selects the last element of a non-empty list;
-- for example, last [1,2,3,4,5] = 5.
-- Show how the function last could be defined in terms of the other
-- library functions introduced in this chapter.
-- Can you think of another possible definition?

-- |
-- >>> last [1,2,3,4,5]
-- 5
last :: [a] -> a
last l = l !! (length l - 1)

-- |
-- >>> last1 [1,2,3,4,5]
-- 5
last1 :: [a] -> a
last1 l = head $ drop (length l - 1) l

-- |
-- >>> last2 [1,2,3,4,5]
-- 5
last2 :: [a] -> a
last2 = (head . reverse)

-- |
-- >>> last3 [1,2,3,4,5]
-- 5
last3 :: [a] -> a
last3 l = l !! max 0 (length l - 1)

-- 5.The library function init removes the last element from a non-empty list;
-- for example, init [1,2,3,4,5] = [1,2,3,4].
-- Show how init could similarly be defined in two different ways.

-- |
-- >>> init [1,2,3,4,5]
-- [1,2,3,4]
init :: [a] -> [a]
init l = take (length l - 1) l

-- |
-- >>> init' [1,2,3,4,5]
-- [1,2,3,4]
init' :: [a] -> [a]
init' [] = []
init' [_] = []
init' (x : xs) = x : init' xs
