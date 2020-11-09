{-# LANGUAGE NoImplicitPrelude #-}

module Part1.Ch03.Exercises where

import GHC.Base
import GHC.Num
import GHC.List

-- 3.11Exercises
-- 1.What are the types of the following values?

-- |
-- >>> str
-- "abc"
str :: [Char]
str = ['a', 'b', 'c']

-- |
-- >>> tup
-- ('a','b','c')
tup :: (Char, Char, Char)
tup = ('a', 'b', 'c')

-- |
-- >>> tups
-- [(False,'O'),(True,'1')]
tups :: [(Bool, Char)]
tups = [(False, 'O'), (True, '1')]

-- |
-- >>> tup'
-- ([False,True],"01")
tup' :: ([Bool], [Char])
tup' = ([False, True], ['0', '1'])

funcs :: [[a] -> [a]]
funcs = [tail, init, reverse]

-- 2.Write down definitions that have the following types;
-- it does not matter what the definitions actually do as long as they are type correct.

-- |
-- >>> bools
-- [True,True,False]
bools :: [Bool]
bools = [True, True, False]

-- |
-- >>> nums
-- [[1,2],[3,4]]
nums :: [[Int]]
nums = [[1, 2], [3, 4]]

add :: Int -> Int -> Int -> Int
add _ _ _ = 9

-- |
-- >>> copy 'a'
-- ('a','a')
copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

-- 3.What are the types of the following functions?

-- |
-- >>> second [1,2,3]
-- 2
second :: [a] -> a
second xs = head (tail xs)

-- |
-- >>> swap ('a',1)
-- (1,'a')
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- |
-- >>> pair 'a' 1
-- ('a',1)
pair :: a -> b -> (a, b)
pair x y = (x, y)

-- |
-- >>> double' 3
-- 6
double' :: Num a => a -> a
double' x = x * 2

-- |
-- >>> palindrome "tattarrattat"
-- True
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

-- |
-- >>> twice (+ 1) 1
-- 3
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Hint: take care to include the necessary class constraints in the types if
-- the functions are defined using overloaded operators.

-- 4.Check your answers to the preceding three questions using GHCi.
-- DONE

-- 5.Why is it not feasible in general for function types to be instances of the Eq class?
-- When is it feasible?
-- Hint: two functions of the same type are equal if they always return equal results for equal arguments.

-- Enumerating all possible inputs and all possible outputs is not always feasible.
