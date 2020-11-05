{-# LANGUAGE NoImplicitPrelude #-}

-- | 8 Declaring types and classes
module Part1.Ch08.Ch08 where

import GHC.Base hiding (Nat)
import GHC.Num ((+), (-), (*))
import GHC.Real (div, (^))
import Data.List (head)
import GHC.Float (pi)

-- | 8.1 Type declarations
type Stringgg = [Char]
type Pos = (Int,Int)

type Trans = Pos -> Pos

type Pair a = (a,a)

type Assoc k v = [(k,v)]

find0 :: Eq k => k -> Assoc k v -> v
find0 k t = head [v | (k',v) <- t, k == k']

-- | 8.2 Data declarations
data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y)= (x+1,y)
move West (x,y)= (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East= West
rev West= East

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- *Main> :t Circle
-- Circle :: Float -> Shape
-- *Main> :t Rect
-- Rect :: Float -> Float -> Shape

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- | 8.3 Newtype declarations
newtype Nat0 = N0 Int
type Nat1 = Int

data Nat2 = N1 Int

-- | 8.4 Recursive types
data Nat = Zero | Succ Nat
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

addNat :: Nat -> Nat -> Nat
addNat Zero n     = n
addNat (Succ m) n = Succ (addNat m n)

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs0 :: Eq a => a -> Tree a -> Bool
occurs0 x (Leaf y) = x == y
occurs0 x (Node l y r) = x == y || occurs0 x l || occurs0 x r

-- |
-- >>> flatten t
-- [1,3,4,5,6,7,9]
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) | x == y = True
                      | x < y  = occurs x l
                      | otherwise = occurs x r

-- Trees in computing come in many different forms,
-- we can declare types for trees that have:

-- | data only in their leaves
data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a)

-- | data only in their nodes
data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a)

-- | data of different types in their leaves and nodes
data Tree3 a b = Leaf3 a | Node3 (Tree3 a b) b (Tree3 a b)

-- | or have a list of subtrees
data Tree4 a = Node4 a [Tree4 a]

-- | 8.5 Class and instance declarations
class Eq0 a where
    (===), (/==) :: a -> a -> Bool

    x /== y = not (x === y)

instance Eq0 Bool where
    False === False = True
    True  === True  = True
    _     === _     = False

-- class Eq0 a => Ord a where ...
