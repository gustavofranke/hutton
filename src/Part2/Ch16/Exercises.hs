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

