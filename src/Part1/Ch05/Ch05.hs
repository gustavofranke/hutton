{-# LANGUAGE NoImplicitPrelude #-}

-- | 5 List comprehensions
module Part1.Ch05.Ch05 where

import Data.Foldable hiding (find, length)
import GHC.Base (Bool (False, True), Char, Eq, Int, Ord, String, (&&), (<=), (==), (>=))
import GHC.List hiding (and, length, sum)
import GHC.Real (mod)

-- | 5.1 Basic concepts
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

length :: [a] -> Int
length xs = sum [1 | _ <- xs]

-- | 5.2 Guards
-- >>> factors 15
-- [1,3,5,15]
--
-- >>> factors 7
-- [1,7]
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

-- |
-- >>> prime 15
-- False
--
-- >>> prime 7
-- True
prime :: Int -> Bool
prime n = factors n == [1, n]

-- |
-- >>> primes 40
-- [2,3,5,7,11,13,17,19,23,29,31,37]
primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], prime x]

-- |
-- >>> find 'b' [('a',1),('b',2),('c',3),('b',4)]
-- [2,4]
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-- | 5.3 The zip function
-- >>> pairs [1,2,3,4]
-- [(1,2),(2,3),(3,4)]
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- |
-- >>> sorted [1,2,3,4]
-- True
--
-- >>> sorted [1,3,2,4]
-- False
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

-- |
-- >>> positions False [True, False, True, False]
-- [1,3]
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

-- | 5.4 String comprehensions
-- >>> lowers "Haskell"
-- 6
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

-- |
-- >>> count 's' "Mississippi"
-- 4
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']
