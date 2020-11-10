{-# LANGUAGE NoImplicitPrelude #-}

module Part1.Ch06.Exercises where

import GHC.Enum
import GHC.Base hiding (foldr)
import GHC.Num
import Data.List
import GHC.Real
import Data.Tuple

-- 6 Recursive functions
-- 6.8 Exercises

-- 1. How does the recursive version of the factorial function behave if applied to a negative argument, such as (-1)?
-- INFINITE LOOP!
-- Modify the definition to prohibit negative arguments by adding a guard to the recursive case.
fac' :: Int -> Int
fac' 0 = 1
fac' n | n > 0 = n * fac' (n -1)

-- | n < 0  = 0
-- | n == 0 = 1
-- | otherwise = n * fac' (n-1)

-- 2. Define a recursive function sumdown :: Int -> Int
-- that returns the sum of the non-negative integers from a given value down to zero.
-- For example, sumdown 3 should return the result 3+2+1+0 = 6.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown x = x + sumdown (x -1)

-- 3. Define the exponentiation operator ^ for non-negative integers
-- using the same pattern of recursion as the multiplication operator *,
-- and show how the expression 2 ^ 3 is evaluated using your definition.
exp0 :: (Integral b, Num a) => a -> b -> a
_ `exp0` 0 = 1
x `exp0` y = x * (x `exp0` (y -1))

-- 4. Define a recursive function euclid :: Int -> Int -> Int
-- that implements Euclid’s algorithm for calculating the greatest common divisor
-- of two non-negative integers:
-- if the two numbers are equal, this number is the result;
-- otherwise, the smaller number is subtracted from the larger,
-- and the same process is then repeated. For example:
-- > euclid 6 27
-- 3
euclid :: Int -> Int -> Int
euclid x y
  | x == y = x
  | x < y = euclid x (y - x)
  | x > y = euclid (x - y) y

-- 5. Using the recursive definitions given in this chapter,
-- show how
-- length [1,2,3],
lengthCalc =
  let a = 1 + length [2, 3]
      _ = 1 + (1 + length [3])
      _ = 1 + (1 + (1 + length []))
      b = 1 + (1 + (1 + 0))
  in a == b

-- drop 3 [1,2,3,4,5], and
dropCalc =
  let a = drop 2 [2,3,4,5]
      _ = drop 1 [3,4,5]
      _ = drop 0 [4,5]
      b = [4,5]
  in a == b

-- init [1,2,3]
-- are evaluated.
initCalc =
  let a = 1 : init [2, 3]
      _ = 1 : (2 : init [3])
      b = 1 : (2 : [])
  in a == b

-- 6. Without looking at the definitions from the standard prelude,
-- define the following library functions on lists using recursion.
-- a. Decide if all logical values in a list are True:
-- and :: [Bool] -> Bool
and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

and'' :: [Bool] -> Bool
and'' [] = True
and'' (False : _) = False
and'' (True : xs) = and'' xs

-- b. Concatenate a list of lists:
-- concat :: [[a]] -> [a]
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x : xs) = x ++ (concat' xs)

-- c. Produce a list with n identical elements:
-- replicate :: Int -> a -> [a]
replicate'' :: Int -> a -> [a]
replicate'' n _ | n <= 0 = []
replicate'' n elem = elem : replicate'' (n -1) elem

-- d. Select the nth element of a list:
-- (!!) :: [a] -> Int -> a
(<!!>) :: [a] -> Int -> a
(<!!>) xs i = go xs i 0
  where
    go (l : ls) n acc = if n == acc then l else go ls n (acc + 1)

bangbang :: (Eq a1, Num a1, Enum a1) => [a2] -> a1 -> a2
bangbang xs n = head $ map (snd) $ filter (\(i, e) -> i == n) $ zip [0 ..] xs

bangbang' :: (Num a1, Enum a1) => [a2] -> a1 -> a2
bangbang' xs n = last $ map (snd) $ zip [0 .. n] xs

-- e. Decide if a value is an element of a list:
-- elem :: Eq a => a -> [a] -> Bool
-- Note: most of these functions are defined in the prelude using other library functions
-- rather than using explicit recursion, and are generic functions rather than being specific to the type of lists.
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x : xs) = if e == x then True else elem' e xs

-- 7. Define a recursive function merge :: Ord a => [a] -> [a] -> [a]
-- that merges two sorted lists to give a single sorted list. For example:
-- > merge [2,5,6] [1,3,4]
-- [1,2,3,4,5,6]
-- Note: your definition should not use other functions on sorted lists such as insert or isort,
-- but should be defined using explicit recursion.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) =
  let rec = merge xs ys
      uns = if (x < y) then x : y : rec else y : x : rec
   in (sort uns) -- TODO: can I use sort???

-- 8. Using merge, define a function msort :: Ord a => [a] -> [a] that implements merge sort,
-- in which the empty list and singleton lists are already sorted,
-- and any other list is sorted by merging together the two lists
-- that result from sorting the two halves of the list separately.
-- Hint: first define a function halve :: [a] -> ([a],[a]) that splits a list into two halves whose lengths differ by at most one.
-- halve :: [a] -> ([a],[a])
-- msort :: Ord a => [a] -> [a]
-- msort [] = []
-- msort [x] = [x]
-- TODO: come back
-- 9. Using the five-step process, construct the library functions that:
-- a.calculate the sum of a list of numbers;
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + (sum' xs)

-- b. take a given number of elements from the start of a list;
take' :: Int -> [a] -> [a]
take' n (x : xs) =
  let go i (l : ls) acc = if acc < i then l : (go i ls (acc + 1)) else []
   in go n (x : xs) 0

-- c.select the last element of a non-empty list.
last''' :: [a] -> a
last''' (x : []) = x
last''' (x : xs) = last''' xs
