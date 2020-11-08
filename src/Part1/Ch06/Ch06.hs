{-# LANGUAGE NoImplicitPrelude #-}

-- | 6 Recursive functions
module Part1.Ch06.Ch06 where

import Data.List hiding (drop, init, insert, length, product, reverse, zip)
import GHC.Base (Int, Ord, otherwise, (<=))
import GHC.Num

-- | 6.1 Basic concepts
-- >>> fac 3
-- 6
fac :: Int -> Int
fac n = product [1 .. n]

fac' :: Int -> Int
fac' 0 = 1
fac' n = n * fac' (n -1)

-- | 6.2 Recursion on lists
-- product [2,3,4]
-- 24
product :: Num a => [a] -> a
product [] = 1
product (n : ns) = n * product ns

length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs

-- |
-- >>> reverse [1,2,3]
-- [3,2,1]
reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

-- (++0) :: [a] -> [a] -> [a]
-- [] ++0 ys    = ys
-- (x:xs) ++0 ys = x : (xs ++0 ys)

-- |
-- >>> insert 3 [1,2,4,5]
-- [1,2,3,4,5]
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

-- | 6.3 Multiple arguments
-- >>> zip ['a','b','c'] [1,2,3,4]
-- [('a',1),('b',2),('c',3)]
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_ : xs) = drop (n -1) xs

-- | 6.4 Multiple recursion
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n -2) + fib (n -1)

-- 6.6 Advice on recursion
init :: [a] -> [a]
init [_]= []
init (x:xs) = x : init xs
