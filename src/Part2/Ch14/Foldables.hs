module Part2.Ch14.Foldables where

import Data.Monoid
import Data.Foldable
-- 14.1 Monoids
test0 = mconcat [Sum 2, Sum 3, Sum 4] -- Sum {getSum = 9}
test1 = mconcat [Product 2, Product 3, Product 4] -- Product {getProduct = 24}
test2 = mconcat [All True, All True, All True] -- All {getAll = True}
test3 = mconcat [Any False, Any False, Any False] -- Any {getAny = False}
-- 14.2 Foldables
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
    fold (Leaf x) = x
    fold (Node l r) = fold l `mappend` fold r

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap f (Leaf x) = f x
    foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f v (Leaf x) = f x v
    foldr f v (Node l r) = foldr f (foldr f v r) l

    -- foldl :: (a -> b -> a) -> a -> Tree b -> a
    foldl f v (Leaf x) = f v x
    foldl f v (Node l r) = foldl f (foldl f v l) r

test4 = getSum (foldMap Sum [1..10]) -- 55
test5 = getProduct (foldMap Product [1..10]) -- 3628800

tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

-- Then evaluating foldr (+) 0 tree gives the result 1+(2+(3+0)),
-- in which the additions are performed from right-to-left,
-- whereas foldl (+) 0 tree gives ((0+1)+2)+3,
-- in which they are performed from left-to-right
test6 = foldr (+) 0 tree -- 6
test7 = foldl (+) 0 tree -- 6

-- Other primitives and defaults
test8 = null [] -- True
test9 = null (Leaf 1) -- False
test10 = length [1..10] -- 10
test11 = length (Node (Leaf 'a') (Leaf 'b')) -- 2
test12 = foldr1 (+) [1..10] -- 55
test13 = foldl1 (+) (Node (Leaf 1) (Leaf 2)) -- 3

--  Generic functions
average0 :: [Int] -> Int
average0 ns = sum ns `div` length ns

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

test14 = average [1..10] -- 5
test15 = average (Node (Leaf 1) (Leaf 3)) -- 2
test16 = and [True,False,True] -- False
test17 = or (Node (Leaf True) (Leaf False)) -- True
test18 = all even [1,2,3] -- False
test19 = any even (Node (Leaf 1) (Leaf 2)) -- True
test20 = concat ["ab","cd","ef"] -- "abcdef"
test21 = concat (Node (Leaf [1,2]) (Leaf [3])) -- [1,2,3]

-- 14.3 Traversables
dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n-1) else Nothing

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x)   = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)

instance Traversable Tree where
    -- traverse :: Applicative f =>
    -- (a -> f b) -> Tree a -> f (Tree b)
    traverse g (Leaf x) = pure Leaf <*> g x
    traverse g (Node l r) = pure Node <*> traverse g l <*> traverse g r

test22 = traverse dec [1,2,3] -- Just [0,1,2]
test23 = traverse dec [2,1,0] -- Nothing
test24 = traverse dec (Node (Leaf 1) (Leaf 2)) -- Just (Node (Leaf 0) (Leaf 1))
test25 = traverse dec (Node (Leaf 0) (Leaf 1)) -- Nothing

-- Other primitives and defaults
test26 = sequenceA [Just 1, Just 2, Just 3] -- Just [1,2,3]
test27 = sequenceA [Just 1, Nothing, Just 3] -- Nothing
test28 = sequenceA (Node (Leaf (Just 1)) (Leaf (Just 2))) -- Just (Node (Leaf 1) (Leaf 2))
test29 = sequenceA (Node (Leaf (Just 1)) (Leaf Nothing)) -- Nothing
 