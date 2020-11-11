{-# LANGUAGE NoImplicitPrelude #-}

module Part1.Ch07.Exercises where

import GHC.Real
import GHC.Num
import GHC.Base hiding (map)
import Data.Foldable (sum)
import Data.Foldable hiding (all, foldl, foldr, concatMap, length, null)
import GHC.List hiding (and, dropWhile, filter, map, or, takeWhile, iterate, sum)
import Part1.Ch07.Ch07 hiding (map, filter)
import Part1.Ch07.Transmitter hiding (encode, int2bin, chop8, decode, transmit)
import Data.Char

-- 7.9 Exercises

-- | 1. Show how the list comprehension [f x | x <- xs, p x]
-- can be re-expressed using the higher-order functions map and filter.

-- |
-- >>> comprehension [1 .. 10] (\x -> x `mod` 2 == 0) ((+) 5)
-- [7,9,11,13,15]
comprehension :: [t] -> (t -> Bool) -> (t -> a) -> [a]
comprehension xs p f = [f x | x <- xs, p x]

-- |
-- >>> highorderfunc  [1 .. 10] (\x -> x `mod` 2 == 0) ((+) 5)
-- [7,9,11,13,15]
highorderfunc :: [t] -> (t -> Bool) -> (t -> a) -> [a]
-- highorderfunc xs p f = map f $ filter p xs
highorderfunc xs p f = (map f . filter p) xs

-- 2. | Without looking at the definitions from the standard prelude,
-- define the following higher-order library functions on lists.
--
-- a. Decide if all elements of a list satisfy a predicate:
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

-- b. Decide if any element of a list satisfies a predicate:
any :: (a -> Bool) -> [a] -> Bool
any p xs = or $ map p xs

-- c. Select elements from a list while they satisfy a predicate:
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
-- takeWhile p (x : xs) = if p x then x : takeWhile p xs else takeWhile p xs
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = []

-- d. Remove elements from a list while they satisfy a predicate:
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
-- dropWhile p (x : xs) = if p x then dropWhile p xs else x : dropWhile p xs
dropWhile p (x : xs)
  | p x = dropWhile p xs
  | otherwise = x : dropWhile p xs

-- Note: in the prelude the first two of these functions are generic functions
-- rather than being specific to the type of lists.

-- 3. Redefine the functions map f and filter p using foldr.
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x : xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> if p x then x : xs else xs) []

-- 4. | Using foldl, define a function dec2int :: [Int] -> Int
-- that converts a decimal number into an integer.
-- For example:
-- >>> dec2int [2,3,4,5]
-- 2345
dec2int :: [Int] -> Int
dec2int = foldl (\y x -> 10 * y + x) 0

-- 5. | Without looking at the definitions from the standard prelude,
-- define the higher-order library function curry that
-- converts a function on pairs into a curried function, and,
-- conversely, the function uncurry that converts a curried function with
-- two arguments into a function on pairs.
-- Hint: first write down the types of the two functions.

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
-- uncurry f t = f (fst t) (snd t)
uncurry f = \(x, y) -> f x y

-- 6. | A higher-order function unfold that encapsulates a simple
-- pattern of recursion for producing a list can be defined as follows:
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

-- That is, the function unfold p h t produces
-- the empty list if the predicate p is true of the argument value,
-- and otherwise produces a non-empty list by applying the function h to this value
-- to give the head, and the function t to generate another argument
-- that is recursively processed in the same way to produce the tail of the list.
-- For example, the function int2bin can be rewritten more compactly using unfold as follows:
int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

-- Redefine the functions chop8, map f and iterate f using unfold.

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)

{-
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs
-}
mapU :: (a -> b) -> [a] -> [b]
-- mapU f (x : xs) = unfold (== []) (const $ f x) (const xs) (x:xs)
mapU f = unfold null (f . head) tail

iterate :: (a -> a) -> a -> [a]
iterate = unfold (const False) id

-- 7. Modify the binary string transmitter example
-- to detect simple transmission errors using the concept of parity bits.
-- 
-- That is, each eight-bit binary number produced during encoding is extended with a parity bit,
-- set to one if the number contains an odd number of ones, and to zero otherwise.
-- In turn, each resulting nine-bit binary number consumed during decoding is checked to
-- ensure that its parity bit is correct, with the parity bit being discarded if this is the case,
-- and a parity error being reported otherwise.
-- Hint: the library function error :: String -> a displays the given string as an error message
-- and terminates the program; the polymorphic result type ensures that error can be used in any context.

onlyOnes :: [Bit] -> [Bit]
onlyOnes = filter (== 1)

isOddOnes :: [Bit] -> Bool
isOddOnes xs = (odd . sum) xs

-- |
-- >>> encode "abc"
-- [1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,1,1,1,0,0,0,1,1,0,0]
encode :: String -> [Bit]
encode = concatMap (\c -> encodeChar c <> [parity c])
  where
    encodeChar = make8 . int2bin . ord
    addParity x
      | x = 1
      | otherwise = 0
    parity c = addParity $ isOddOnes $ onlyOnes $ encodeChar c

chunksOf :: [a] -> [[a]]
chunksOf [] = []
chunksOf encoded = take 9 encoded : chunksOf (drop 9 encoded)

-- |
-- >>> decode [1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,1,1,1,0,0,0,1,1,0,0]
-- "abc"
decode :: [Bit] -> String
decode xs = checkPartity xs >>= decode8C
  where
    decode8C = map (chr . bin2int) . chop8

checkPartity :: [Bit] -> [[Bit]]
checkPartity xs =
  [ let (byt, pb)  = splitAt 8 wpb
        shouldHave = isOddOnes $ onlyOnes byt
        shouldNot  = not shouldHave
    in if shouldHave && pb == [1] || shouldNot  && pb == [0]
        then byt
        else error "parity bit being discarded"
    | wpb <- chunksOf xs
  ]

-- |
-- >>> transmit "higher-order functions are easy"
-- "higher-order functions are easy"
transmit :: String -> String
transmit = decode . channel . encode


-- 8. Test your new string transmitter program from the previous exercise
-- using a faulty communication channel that forgets the first bit,
-- which can be modelled using the tail function on lists of bits.

-- 9. Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternately
-- applies its two argument functions to successive elements in a list, in turn about order.
-- For example:
-- >>> altMap (+10) (+100) [0,1,2,3,4]
-- [10,101,12,103,14]
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g ys = map func (withIndex ys)
  where
    withIndex = zip [(0 :: Int) ..]
    -- func (i, e) = if odd i then f e else g e
    func (i, e)
      | even i = f e
      | otherwise = g e


-- 10. Using altMap, define a function luhn :: [Int] -> Bool
-- that implements the Luhn algorithm from the exercises in chapter 4
-- for bank card numbers of any length.
-- Test your new function using your own bank card.
luhnDouble :: Int -> Int
luhnDouble x = if double > 9 then double - 9 else double
  where
    double = 2 * x

-- > luhn [1, 7, 8, 4]
-- True
-- 
-- > luhn [4, 7, 8, 3]
-- False
luhn :: [Int] -> Bool
luhn xs = func `mod` 10 == 0
  where func = sum $ altMap id luhnDouble (reverse xs)
