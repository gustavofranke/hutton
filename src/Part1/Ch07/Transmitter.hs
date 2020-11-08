{-# LANGUAGE NoImplicitPrelude #-}

module Part1.Ch07.Transmitter where

import Data.Char (chr)
import Data.List hiding (filter, foldr, map)
import GHC.Base hiding (map)
import GHC.Num
import GHC.Real (div, even, mod, (^))
import Part1.Ch07.Ch07 (map)

-- | 7.6 Binary string transmitter
-- Base conversion
type Bit = Int

-- |
-- >>> bin2int0 [1,0,1,1]
-- 13
bin2int0 :: [Bit] -> Int
bin2int0 bits = sum [w * b | (w, b) <- zip weights bits]
  where
    weights = iterate (* 2) 1

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

-- |
-- >>> int2bin 13
-- [1,0,1,1]
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- |
-- >>> make8 [1,0,1,1]
-- [1,0,1,1,0,0,0,0]
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- | Transmission
-- >>> encode "abc"
-- [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- |
-- >>> decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
-- "abc"
decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

-- |
-- >>> transmit "higher-order functions are easy"
-- "higher-order functions are easy"
transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
