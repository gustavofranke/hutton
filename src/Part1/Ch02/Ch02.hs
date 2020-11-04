{-# LANGUAGE NoImplicitPrelude #-}

-- | 2 First steps
module Part1.Ch02.Ch02 where

import Data.Foldable (Foldable, length, product, sum)
import GHC.Base (Int)
import GHC.Enum
import GHC.Num
import GHC.Real (div)
import Part1.Ch01.Ch01 (double)

-- | 2.5 Haskell Scripts
-- >>> quadruple 10
-- 40
quadruple :: Num a => a -> a
quadruple x = double (double x)

-- |
-- >>> factorial 10
-- 3628800
factorial :: (Num a, Enum a) => a -> a
factorial n = product [1 .. n]

-- >>> average [1,2,3,4,5]
-- 3
average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns
