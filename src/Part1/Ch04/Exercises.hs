{-# LANGUAGE NoImplicitPrelude #-}

module Part1.Ch04.Exercises where

import GHC.Base
import GHC.Num
import GHC.List
import GHC.Real

-- 4 Defining functions
-- 4.8 Exercises

-- 1. Using library functions, define a function
-- halve :: [a] -> ([a],[a]) that splits an even-lengthed list into two halves.
-- For example:
-- |
-- >>> halve [1,2,3,4,5,6]
-- ([1,2,3],[4,5,6])
halve :: [a] -> ([a], [a])
halve xs = (take len xs, drop len xs)
  where
    len = length xs `div` 2

-- 2. Define a function third :: [a] -> a that
-- returns the third element in a list that contains at least this
-- many elements using:
-- a. head and tail;
third :: [a] -> a
third = (head . tail . tail)

-- b. list indexing !!;
third' :: [a] -> a
third' = (!! 2)

-- c. pattern matching.
third'' :: [a] -> a
third'' (_ : _ : x : _) = x

-- 3. Consider a function safetail :: [a] -> [a] that behaves in the same way
-- as tail except that it maps the empty list to itself rather than producing
-- an error.
-- Using tail and the function null :: [a] -> Bool that decides if a list is
-- empty or not, define safetail using:
-- a. a conditional expression;
safetail :: [a] -> [a]
safetail xs = if null xs then xs else tail xs

-- b. guarded equations;
safetail' :: [a] -> [a]
safetail' xs
  | null xs = xs
  | otherwise = tail xs

-- c. pattern matching.
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_ : xs) = xs

-- 4. In a similar way to && in section 4.4,
-- show how the disjunction operator || can be defined
-- in four different ways using pattern matching.
disj :: Bool -> Bool -> Bool
True `disj` True = True
True `disj` False = True
False `disj` True = True
False `disj` False = False

disj0 :: Bool -> Bool -> Bool
False `disj0` False = False
_ `disj0` _ = True

disj1 :: Bool -> Bool -> Bool
False `disj1` a = a
True `disj1` _ = True

disj2 :: Bool -> Bool -> Bool
a `disj2` b
  | a == b = a
  | otherwise = True

-- 5. Without using any other library functions or operators,
-- show how the meaning of the following pattern matching definition for
-- logical conjunction && can be formalised using conditional expressions:
-- True && True = True
-- _ && _ = False
-- Hint: use two nested conditional expressions.
conj0 :: Bool -> Bool -> Bool
conj0 a b =
  if a == True
    then if b == True then True else False
    else False

-- 6. Do the same for the following alternative definition,
-- and note the difference in the number of conditional expressions that are
-- required:
-- True && b= b
-- False && _ = False
conj1 :: Bool -> Bool -> Bool
conj1 a b =
  if a == True
    then b
    else False

-- 7. Show how the meaning of the following curried function definition can be
-- formalised in terms of lambda expressions:
-- mult :: Int -> Int -> Int -> Int
-- mult x y z = x*y*z
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z

-- 8. The Luhn algorithm is used to check bank card numbers
-- for simple errors such as mistyping a digit,
-- and proceeds as follows:
-- consider each digit as a separate number;
-- moving left, double every other number from the second last;
-- subtract 9 from each number that is now greater than 9;
-- add all the resulting numbers together;
-- if the total is divisible by 10, the card number is valid.
-- Define a function
-- luhnDouble :: Int -> Int
-- that doubles a digit and subtracts 9 if the result is greater than 9.
-- For example:
-- > luhnDouble 3
-- 6
-- > luhnDouble 6
-- 3
luhnDouble :: Int -> Int
luhnDouble x = if double > 9 then double - 9 else double
  where
    double = 2 * x

luhnDouble' :: Int -> Int
luhnDouble' x =
  let double = 2 * x
   in if double > 9 then double - 9 else double

-- Using luhnDouble and the integer remainder function mod, define a function
-- luhn :: Int -> Int -> Int -> Int -> Bool that decides if a four-digit bank
-- card number is valid.
-- For example:
-- > luhn 1 7 8 4
-- True
-- 
-- > luhn 4 7 8 3
-- False
-- In the exercises for chapter 7 we will consider a more general version of
-- this function that accepts card numbers of any length.
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d =
  let doubles = (luhnDouble a) + b + (luhnDouble c) + d
   in doubles `mod` 10 == 0
