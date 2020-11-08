{-# LANGUAGE NoImplicitPrelude #-}

-- | 5.5 The Caesar cipher
module Part1.Ch05.Cipher where

import Data.Char (chr, isLower)
import Data.List (drop, head, minimum, sum, take, zip)
import GHC.Base (Char, Float, Int, String, ord, otherwise, (++))
import GHC.Num
import GHC.Real (fromIntegral, mod, (/), (^))
import Part1.Ch05.Ch05 (count, lowers, positions)

-- |
-- >>> let2int 'a'
-- 0
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- |
-- >>> int2let 0
-- 'a'
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

-- |
-- >>> shift 3 'a'
-- 'd'
--
-- >>> shift 3 'z'
-- 'c'
--
-- >>> shift (-3) 'c'
-- 'z'
--
-- >>> shift 3 ' '
-- ' '
shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

-- |
-- >>> encode 3 "haskell is fun"
-- "kdvnhoo lv ixq"
--
-- >>> encode (-3) "kdvnhoo lv ixq"
-- "haskell is fun"
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

--- | cracking
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- |
-- >>> percent 5 15
-- 33.333336
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
  where
    n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

-- |
-- >>> rotate 3 [1,2,3,4,5]
-- [4,5,1,2,3]
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- |
-- >>> crack "kdvnhoo lv ixq"
-- "haskell is fun"
--
-- >>> crack "vscd mywzboroxcsyxc kbo ecopev"
-- "list comprehensions are useful"
crack :: String -> String
crack xs = encode (- factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs
