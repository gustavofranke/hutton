-- {-# LANGUAGE NoImplicitPrelude #-}

module Part2.Ch15.Exercises where

import Prelude hiding (take, repeat)

-- 15.9 Exercises

-- 1. Identify the redexes in the following expressions,
-- and determine whether each redex is innermost, outermost,
-- neither, or both:
-- 1 + (2*3)
-- 1 + (6)

-- (1+2) * (2+3)
-- (3) * (2+3) -- I
-- (1+2) * (5) -- I

-- fst (1+2, 2+3)
-- (1+2)  -- O
-- fst (3, 2+3) -- I
-- fst (1+2, 5) -- I

-- (\x -> 1 + x) (2*3)
-- (\x -> 1 + x) (6) -- I

-- 2. Show why outermost evaluation is preferable to innermost
-- for the purposes of evaluating the expression fst (1+2,2+3).

-- fst (1+2, 2+3)
-- (1+2)
-- (3)
-- vs
-- fst (1+2, 2+3)
-- fst (3, 2+3)
-- fst (3, 5)
-- 3

-- 3. Given the definition mult = \x -> (\y -> x * y),
-- show how the evaluation of mult 3 4 can be broken down into
-- four separate steps.

-- mult 3 4
-- (\x -> (\y -> x * y)) 3 4
-- (\y -> 3 * y) 4
-- 3 * 4
-- 12

-- 4. Using a list comprehension, define an expression
-- fibs :: [Integer] that generates the infinite sequence of Fibonacci numbers
-- 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...

-- using the following simple procedure:
-- the first two numbers are 0 and 1;
-- the next is the sum of the previous two;
-- return to the second step.
-- Hint: make use of the library functions zip and tail.
-- Note that numbers in the Fibonacci sequence quickly become large,
-- hence the use of the type Integer of arbitrary-precision integers above.

-- |
-- >>> take 10 $ fibs
-- [0,1,1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = 0 : 1 : go 0 (1 :: Integer)
  where
    go a b = [y | x <- [a + b]
                , y <- x : go b (a + b)]

-- |
-- >>> take 10 $ fibs1
-- [0,1,1,2,3,5,8,13,21,34]
fibs1 :: [Integer]
fibs1 = 0 : 1 : go 0 (1 :: Integer)
  where
    go a b = (a + b) : go b (a + b)

-- 5. Define appropriate versions of the library functions
repeat :: a -> [a]
repeat x = xs
  where
    xs = x : xs

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x : xs) = x : take (n -1) xs

replicate :: Int -> a -> [a]
replicate n = take n . repeat

-- for the following type of binary trees:
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

repeat0 :: a -> Tree a
repeat0 x = xs
  where
    xs = Node xs x xs

take0 :: Int -> Tree a -> Tree a
take0 0 _ = Leaf
take0 _ Leaf = Leaf
take0 n (Node tl e tr) = Node (take0 (n -1) tl) e (take0 (n -1) tr)

replicate0 :: Int -> a -> Tree a
replicate0 n = take0 n . repeat0


-- 6. Newton’s method for computing the square root of
-- a (non-negative) floating-point number n can be expressed as follows:
-- 
-- start with an initial approximation to the result;
-- given the current approximation a, the next approximation is defined by
-- the function next a = (a + n/a) / 2;
-- repeat the second step until the two most recent approximations are
-- within some desired distance of one another, at which point
-- the most recent value is returned as the result.
-- Define a function sqroot :: Double -> Double that implements this procedure.
-- Hint: first produce an infinite list of approximations using
-- the library function iterate.
-- 
-- For simplicity, take the number 1.0 as the initial approximation,
-- and 0.00001 as the distance value.

-- |
-- >>> sqroot 3
-- 1.75
-- >>> sqroot 2
-- 1.4166666666666665
sqroot :: Double -> Double
sqroot n = (resu . step) approximation
  where
    resu (h:s:as) = if s - h <= distance then s else resu (s:as)
    step a = iterate next a
    next a = (a + n / a) / 2
    approximation = 1.0 :: Double
    distance  = 0.00001 :: Double

