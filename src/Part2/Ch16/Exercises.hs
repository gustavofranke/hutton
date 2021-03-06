module Part2.Ch16.Exercises where

import Prelude hiding (all, (++), take, drop)
-- 16.9 Exercises
-- 1. Show that add n (Succ m) = Succ (add n m), by induction on n.
data Nat = Zero | Succ Nat deriving Eq

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

-- Base case:
addBaseCase :: Bool
addBaseCase =
  let a = add Zero Zero -- applying add 
      b = Zero
   in a == b

-- Inductive case:
addInductiveCase :: Nat -> Bool
addInductiveCase n =
  let a = add (Succ n) Zero -- applying add
      _ = Succ (add n Zero) -- induction hypothesis
      b = Succ n
   in a == b

-- | add n (Succ m) = Succ (add n m)
-- >>> inductionOnN (Succ Zero)
-- True
inductionOnN :: Nat -> Bool
inductionOnN m =
  let a = add Zero (Succ m)
      b = Succ m
   in a == b


-- | add n (Succ m) = Succ (add n m)
-- >>> inductionOnN' (Succ Zero)
-- True
inductionOnN' :: Nat -> Bool
inductionOnN' m =
  let a = Succ (add Zero m)
      b = Succ m
   in a == b

-- 2. Using this property, together with add n Zero = n,
-- show that addition is commutative, add n m = add m n, by induction on n.

-- Hypothesis: add Zero m = add m Zero
-- Prop: add n (Succ m) = Succ (add n m)
-- Prop: add Zero (Succ m) = Succ (add Zero m)

-- |
-- >>> commutativity (Succ Zero)
-- True
commutativity :: Nat -> Bool
commutativity m = 
    let a = add Zero m
        b = m
     in a == b

-- |
-- >>> commutativity' (Succ Zero)
-- True
commutativity' :: Nat -> Bool
commutativity' m =
    let a = add m Zero
        b = add Zero m
     in a == b

-- 3. Using the following definition for the library function that
-- decides if all elements of a list satisfy a predicate
all :: (t -> Bool) -> [t] -> Bool
all p [] = True
all p (x:xs) = p x && all p xs

-- complete the proof of the correctness of replicate by showing that
-- it produces a list with identical elements,
-- all (== x) (replicate n x), by induction on
-- Hint: show that the property is always True.
prop3 :: Eq a => Int -> a -> Bool
prop3 n x =
    let a = all (== x) (replicate n x)
        b = (== x) x && all (== x) (replicate (n - 1) x)
    in a == b

-- 4. Using the definition
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
-- verify the following two properties, by induction on xs:
-- xs ++ [] = xs
-- xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
-- Hint: the proofs are similar to those for the add function.

-- Prop 1:
-- [] ++ [] = [] -- applying [] ++ ys = ys, with ys = []

-- Prop 2:
-- [] ++ (ys ++ zs) = ([] ++ ys) ++ zs

-- [] ++ (ys ++ zs) -- applying [] ++ ys = ys, with ys = (ys ++ zs)
--       (ys ++ zs)
-- ys ++ zs

-- ([] ++ ys) ++ zs -- applying [] ++ ys = ys
-- ys ++ zs

-- 5. Using the above definition for ++, together with
take :: (Eq t, Num t) => t -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x : xs) = x : take (n -1) xs

drop :: (Eq t, Num t) => t -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_ : xs) = drop (n -1) xs

-- show that take n xs ++ drop n xs = xs,
-- by simultaneous induction on the integer n, and the list xs.
-- Hint: there are three cases,
-- one for each pattern of arguments in the definitions of take and drop.

-- Prop:
-- take 0 xs ++ drop 0 xs
-- []        ++ xs
-- xs

-- 6. Given the type declaration
data Tree = Leaf Int | Node Tree Tree
-- show that the number of leaves in such a tree is always
-- one greater than the number of nodes, by induction on trees.
-- Hint: start by defining functions that count the number of leaves
-- and nodes in a tree.

nleaves :: Tree -> Int
nleaves (Leaf _) = 1
nleaves (Node t1 t2) = nleaves t1 + nleaves t2

nnodes:: Tree -> Int
nnodes (Leaf _) = 0
nnodes (Node t1 t2) = 1 + nnodes t1 + nnodes t2

-- Prop: #leaves = #nodes + 1
-- |
-- >>> prop 6
-- True
prop :: Int -> Bool
prop x = nleaves (Leaf x) == 1 + nnodes (Leaf x)

-- 7. Verify the functor laws for the Maybe type.
-- Hint: the proofs proceed by case analysis, and do not require the use of induction.
-- Impl:
-- fmap :: (a -> b) -> Maybe a -> Maybe b
-- fmap _ Nothing  = Nothing
-- fmap g (Just x) = Just (g x)

-- Law : fmap id = id

-- fmap id Nothing
-- Nothing

-- fmap id (Just x)
-- Just (id x)
-- Just x

-- Law : fmap (g . h) = fmap g . fmap h

-- fmap (g . h)
-- fmap (g . h) Nothing
-- Nothing

-- fmap g . fmap h
-- fmap (g . h) (Just x)
-- Just (g . h x)

-- fmap (g . h)
-- fmap (g . h) (Just x)
-- Just (g . h x)

-- 8. Given the type and instance declarations below,
-- verify the functor laws for the Tree type, by induction on trees.
data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a)
instance Functor Tree1 where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf1 x) = Leaf1 (g x)
    fmap g (Node1 l r) = Node1 (fmap g l) (fmap g r)

-- Law : fmap id = id
-- fmap id (Leaf1 x)
-- Leaf1 (id x)

-- fmap id (Node1 l r)
-- Node1 (fmap id l) (fmap id r)
-- Node1 (fmap id Leaf1) (fmap id Leaf1)
-- Node1 (Leaf1 id) (Leaf1 id)

-- Law : fmap (g . h) = fmap g . fmap h
-- fmap (g . h)
-- fmap (g . h) (Leaf1 x)
-- Leaf1 ((g . h) x)

-- fmap g . fmap h
-- fmap g (Leaf1 x) . fmap h (Leaf1 x)
-- Leaf1 (g x) . Leaf1 (h x)

