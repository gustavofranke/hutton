{-# LANGUAGE NoImplicitPrelude #-}

module Part1.Ch01.Exercises where

import GHC.Base
import GHC.List hiding (product)
import GHC.Num

-- 1. Introduction
-- 1.7 Exercises

-- | 1. Give another possible calculation for the result of double (double 2).
double :: Num a => a -> a
double x = x + x

-- |
-- >>> doubleCalcI
-- True
doubleCalcI :: Bool
doubleCalcI =
  let a = double (double 2) -- inner double
      _ = double (2 + 2) -- outer double
      _ = (2 + 2) + (2 + 2) -- first +
      _ = 4 + (2 + 2) -- second +
      _ = 4 + 4 -- +
      f = 8 :: Int --
   in a == f

-- |
-- >>> doubleCalcO
-- True
doubleCalcO :: Bool
doubleCalcO =
  let a = double (double 2) --
      _ = (double 2) + (double 2)
      _ = (double 2) + (2 + 2)
      _ = (double 2) + 4
      _ = (2 + 2) + 4
      _ = 4 + 4
      f = 8
   in a == f

another :: Num a => a -> a
another = double . double

-- | 2.Show that sum [x] = x for any number x.
-- |
-- >>> ex2Calc 5
-- True
ex2Calc :: (Eq p, Num p) => p -> Bool
ex2Calc x =
  let a = sum [x]
      _ = x + sum []
      _ = x + 0
      b = x
   in a == b

ex2 :: (Eq a, Num a) => a -> Bool
ex2 x = sum [x] == x

ex2Proof :: Bool
ex2Proof = all ex2 [1 .. 1000]

-- | 3.Define a function product that produces the product of
-- a list of numbers, and show using your definition that
-- >>> product [2,3,4]
-- 24
product :: Num a => [a] -> a
product [] = 1
product (x : xs) = x * product xs

product' :: Num a => [a] -> a
product' = foldr (*) 1

-- a :: [Integer]
-- a = map (\p -> p [2, 3, 4]) [product, product', product'']

-- 4.How should the definition of the function qsort be modified so that
-- it produces a reverse sorted version of a list?
qsortR :: Ord a => [a] -> [a]
qsortR [] = []
qsortR (x : xs) = qsort larger ++ [x] ++ qsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- 5.What would be the effect of replacing <= by < in the original
-- definition of qsort? Hint: consider the example qsort [2,2,3,1,1].

-- |
-- >>> qsort [2,2,3,1,1]
-- [1,2,3]
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b > x]

-- It'll remove duplicates
