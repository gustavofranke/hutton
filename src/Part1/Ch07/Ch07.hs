{-# LANGUAGE NoImplicitPrelude #-}

-- 7 | Higher-order functions
module Part1.Ch07.Ch07 where

import Data.Char (chr)
import Data.List hiding (filter, foldr, map)
import GHC.Base hiding (map)
import GHC.Num
import GHC.Real (div, even, mod, (^))

-- | 7.1 Basic concepts
add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)

-- |
-- >>> twice (*2) 3
-- 12
--
-- >>> twice reverse [1,2,3]
-- [1,2,3]
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- | 7.2 Processing lists
-- >>> map (+1) [1,3,5,7]
-- [2,4,6,8]
--
-- >>> map even [1,2,3,4]
-- [False,True,False,True]
--
-- >>> map reverse ["abc","def","ghi"]
-- ["cba","fed","ihg"]
map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' f [] = []
map'' f (x : xs) = f x : map'' f xs

-- |
-- >>> filter even [1..10]
-- [2,4,6,8,10]
--
-- >>> filter (> 5) [1..10]
-- [6,7,8,9,10]
--
-- >>> filter (/= ' ') "abc def ghi"
-- "abcdefghi"
filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p [] = []
filter'' p (x : xs)
  | p x = x : filter'' p xs
  | otherwise = filter'' p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^ 2) (filter even ns))

-- 7.3 | The foldr function
-- sum [] = 0
-- sum (x:xs) = x + sum xs

-- product [] = 1
-- product (x:xs) = x * product xs

or' [] = False
or' (x : xs) = x || or xs

and' [] = True
and' (x : xs) = x && and xs

sum'' :: Num a => [a] -> a
sum'' = foldr (+) 0

product'' :: Num a => [a] -> a
product'' = foldr (*) 1

or'' :: [Bool] -> Bool
or'' = foldr (||) False

and'' :: [Bool] -> Bool
and'' = foldr (&&) True

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x : xs) = f x (foldr' f v xs)

length''' :: [a] -> Int
length''' = foldr' (\_ n -> 1 + n) 0

reverse'' :: [a] -> [a]
reverse'' = foldr' snoc []

snoc x xs = xs ++ [x]

-- 7.4 | The foldl function
sumf :: Num a => [a] -> a
sumf = sum' 0
  where
    sum' v [] = v
    sum' v (x : xs) = sum' (v + x) xs

sumfl :: Num a => [a] -> a
sumfl = foldl (+) 0

productfl :: Num a => [a] -> a
productfl = foldl (*) 1

orfl :: [Bool] -> Bool
orfl = foldl (||) False

andfl :: [Bool] -> Bool
andfl = foldl (&&) True

lengthfl :: [a] -> Int
lengthfl = foldl (\n _ -> n + 1) 0

reversefl :: [a] -> [a]
reversefl = foldl (\xs x -> x : xs) []

-- 7.5 | The composition operator
odd0 n = not (even n)

-- twice f x = f(f x)
-- sumsqreven ns = sum (map (^2) (filter even ns))

odd1 n = (not . even) n

odd2 = not . even

twice2 f = f . f

sumsqreven2 ns = sum $ map (^ 2) (filter even ns)

sumsqreven3 = sum . map (^ 2) . filter even
