-- | 2 First steps
module Part1.Ch02.Ch02 where

import Part1.Ch01.Ch01 (double)

-- | 2.5 Haskell Scripts
quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns
