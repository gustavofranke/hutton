{-# LANGUAGE NoImplicitPrelude #-}

module Part1.Ch05.Exercises where

import Data.Char
import Part1.Ch05.Ch05 hiding (factors, positions)
import Part1.Ch05.Cipher hiding (chisqr, encode, freqs)
import GHC.Base --(Int, String, Float, Eq)
import GHC.List hiding (length)
import GHC.Num
import GHC.Real

-- 5 List comprehensions
-- 5.7Exercises

-- 1.Using a list comprehension,
-- give an expression that calculates the sum 12 + 22 + ... 1002 of
-- the first one hundred integer squares.
squares :: [Int]
squares = [x ^ 2 | x <- [1 .. 100]]

-- 2.Suppose that a coordinate grid of size m × n is given by
-- the list of all pairs (x, y) of integers such that 0 <= x <= m and 0 <= y <= n
-- Using a list comprehension, define a function
-- grid :: Int -> Int -> [(Int,Int)]
-- that returns a coordinate grid of a given size. For example:
-- >>> grid 1 2
-- [
-- (0,0),(0,1),(0,2),
-- (1,0),(1,1),(1,2)
-- ]
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- 3.Using a list comprehension and the function grid above, define a function
-- square :: Int -> [(Int,Int)]
-- that returns a coordinate square of size n,
-- excluding the diagonal from (0, 0) to (n, n). For example:
-- > square 2
-- [
-- (0,1),(0,2),
-- (1,0),(1,2),
-- (2,0),(2,1)
-- ]
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4.In a similar way to the function length, show how the library function
-- replicate :: Int -> a -> [a]
-- that produces a list of identical elements can be defined using a list comprehension. For example:
-- > replicate 3 True
-- [True,True,True]
replicate :: Int -> a -> [a]
replicate n x = [x | a <- [1 .. n]]

-- 5.A triple (x, y, z) of positive integers is Pythagorean if
-- it satisfies the equation x2 + y2 = z2.
-- Using a list comprehension with three generators,
-- define a function pyths :: Int -> [(Int,Int,Int)]
-- that returns the list of all such triples whose components are at most a given limit. For example:
-- > pyths 10
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-- pyths :: Int -> [(Int,Int,Int)]
pyths n =
  [ (x, y, z)
    | x <- [1 .. n],
      y <- [1 .. n],
      z <- [1 .. n],
      x ^ 2 + y ^ 2 == z ^ 2
  ]

-- 6.A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself.
-- Using a list comprehension and the function factors,
-- define a function perfects :: Int -> [Int]
-- that returns the list of all perfect numbers up to a given limit. For example:
-- > perfects 500
-- [6,28,496]
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects limit = [a | a <- [1 .. limit], a == sum (filter (/= a) (factors a))]

-- 7.Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]] with two generators
-- can be re-expressed using two comprehensions with single generators.
-- Hint: nest one comprehension within the other and
-- make use of the library function concat :: [[a]] -> [a].

-- 8.Redefine the function positions using the function find.
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

-- positions' :: Eq a => a -> [a] -> [Int]
-- positions x xs = zip xs [0..]

-- 9.The scalar product of two lists of integers xs and ys of length n
-- is given by the sum of the products of corresponding integers: sigma from (i = 0) to (n - 1) of (xs sub i * ys sub i)
-- In a similar manner to chisqr,
-- show how a list comprehension can be used to define a function
-- scalarproduct :: [Int] -> [Int] -> Int
-- that returns the scalar product of two lists. For example:
-- > scalarproduct [1,2,3] [4,5,6]
-- 32
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct os es = sum [o * e | (o, e) <- zip os es]

scalarproduct' :: [Int] -> [Int] -> Int
scalarproduct' xs ys = sum $ map (\(x, y) -> x * y) (zip xs ys)

-- 10.Modify the Caesar cipher program to also handle upper-case letters.

-- >>> let2upper 'A'
-- 0
let2upper :: Char -> Int
let2upper c = ord c - ord 'A'

-- >>> int2upper 0
-- 'A'
int2upper :: Int -> Char
int2upper n = chr (ord 'A' + n)

-- >>> shiftUpsAndLows 3 'a'
-- 'd'
-- >>> shiftUpsAndLows 3 'A'
-- 'D'

-- >>> shiftUpsAndLows 3 'Z'
-- 'C'
-- >>> shiftUpsAndLows 3 'z'
-- 'c'

-- >>> shiftUpsAndLows (-3) 'c'
-- 'z'
-- >>> shiftUpsAndLows (-3) 'C'
-- 'Z'

-- >>> shiftUpsAndLows 3 ' '
-- ''
shiftUpsAndLows :: Int -> Char -> Char
shiftUpsAndLows n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2upper ((let2upper c + n) `mod` 26)
  | otherwise = c

--------
-- >>> encode 3 "haskell is fun"
-- "kdvnhoo lv ixq"
-- >>> encode 3 "HASKELL IS FUN"
-- "KDVNHOO LV IXQ"
-- >>>> encode (-3) "kdvnhoo lv ixq"
-- "haskell is fun"
-- >>> encode (-3) "KDVNHOO LV IXQ"
-- "HASKELL IS FUN"
encode :: Int -> String -> String
encode n xs = [shiftUpsAndLows n x | x <- xs]

-- TODO: this has to handle uppercase too, should call uppers somehow
freqsWithUpsAndLows :: String -> [Float]
freqsWithUpsAndLows xs = if all isUpper $ filter (/= ' ') xs then upps else lows
  where
    lows = [percent (count x xs) (lowers xs) | x <- ['a' .. 'z']]
    upps = [percent (count x xs) (uppers xs) | x <- ['A' .. 'Z']]

uppers :: String -> Int
uppers xs = length [x | x <- xs, x >= 'A' && x <= 'Z']

-- >>> crackWithUpsAndLows "KDVNHOO LV IXQ"
-- >>> "HASKELL IS FUN"
-- xxxx >>> crackWithUpsAndLows "kdvnhoo lv ixq"
-- xxxx "haskell is fun"

-- >>> crackWithUpsAndLows "VSCD MYWZBOROXCSYXC KBO ECOPEV"
-- >>> "LIST COMPREHENSIONS ARE USEFUL"
-- xxxx >>> crackWithUpsAndLows "vscd mywzboroxcsyxc kbo ecopev"
-- xxxx "list comprehensions are useful"
crackWithUpsAndLows :: String -> String
crackWithUpsAndLows xs = encode (- factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqsWithUpsAndLows xs
