module Part1.Ch07.Ch07 where

import Data.Char
import Data.List

---------------------------
-- 7 Higher-order functions
---------------------------
-- 7.1 Basic concepts
add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- 7.2 Processing lists
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' f []     = []
map'' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p []                 = []
filter'' p (x:xs) | p x       = x : filter'' p xs
                  | otherwise = filter'' p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

-- 7.3 The foldr function
-- sum [] = 0
-- sum (x:xs) = x + sum xs

-- product [] = 1
-- product (x:xs) = x * product xs

or' [] = False
or' (x:xs) = x || or xs

and' [] = True
and' (x:xs) = x && and xs

sum'' :: Num a => [a] -> a
sum'' = foldr (+) 0

product'' :: Num a => [a] -> a
product'' = foldr (*) 1

or'' :: [Bool] -> Bool
or'' = foldr (||) False

and'' :: [Bool] -> Bool
and'' = foldr (&&) True

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)

length''' :: [a] -> Int
length''' = foldr' (\_ n -> 1 + n) 0

reverse'' :: [a] -> [a]
reverse'' = foldr' snoc []

snoc x xs = xs ++ [x]

-- 7.4 The foldl function
sumf :: Num a => [a] -> a
sumf = sum' 0
       where
           sum' v [] = v
           sum' v (x:xs) = sum' (v+x) xs

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
reversefl = foldl (\xs x -> x:xs) []

-- 7.5 The composition operator
odd0 n = not (even n)
-- twice f x = f(f x)
-- sumsqreven ns = sum (map (^2) (filter even ns))

odd1 n = (not . even) n

odd2 = not . even

twice2 f = f . f

sumsqreven2 ns = sum $ map (^2) (filter even ns)
sumsqreven3 = sum . map (^2) . filter even

-- 7.6 Binary string transmitter
-- Base conversion
type Bit = Int

bin2int0 :: [Bit] -> Int
bin2int0 bits = sum [w*b | (w,b) <- zip weights bits]
                    where weights = iterate (*2) 1

test0 :: Int
test0 = bin2int0 [1,0,1,1]

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

test1 :: [Bit]
test1 = int2bin 13

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

test2 :: [Bit]
test2 = make8 [1,0,1,1]

-- Transmission

encode1 :: String -> [Bit]
encode1 = concat . map (make8 . int2bin . ord)

test3 :: [Bit]
test3 = encode1 "abc" -- [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

test4 :: String
test4 = decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] -- "abc"

transmit :: String -> String
transmit = decode . channel . encode1

channel :: [Bit] -> [Bit]
channel = id

test5 :: String
test5 = transmit "higher-order functions are easy" -- "higher-order functions are easy"
