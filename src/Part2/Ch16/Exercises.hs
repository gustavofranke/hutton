module Part2.Ch16.Exercises where

import Prelude hiding (all, (++))
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

