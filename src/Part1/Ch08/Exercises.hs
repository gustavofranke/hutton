{-# LANGUAGE InstanceSigs #-}
module Part1.Ch08.Exercises where

-- import Part1.Ch08.Tautology
-- import Part1.Ch08.Ch08 (Eq0)

-- 8 Declaring types and classes
-- 8.9 Exercises

-- 1. In a similar manner to the function add,
-- define a recursive multiplication function
-- mult :: Nat -> Nat -> Nat for the recursive type of natural numbers:
-- Hint: make use of add in your definition.
data Nat = Zero | Succ Nat deriving (Show, Eq)

addNat :: Nat -> Nat -> Nat
addNat Zero n = n
addNat (Succ m) n = Succ (addNat m n)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n -1))

one :: Nat
one = Succ Zero

two :: Nat
two = Succ (Succ Zero)

three :: Nat
three = Succ (Succ (Succ Zero))

test :: Nat -- 2 + 1 = 3
test = addNat two one

-- showing that 2 + 1 = 3 proceeds as follows:
testa :: Nat
testa = addNat (Succ (Succ Zero)) (Succ Zero) -- applying addNat

testb :: Nat
testb = Succ (addNat (Succ Zero) (Succ Zero)) -- applying addNat

testc :: Nat
testc = Succ (Succ (addNat Zero (Succ Zero))) -- applying addNat

testd :: Nat
testd = Succ (Succ (Succ Zero))

test1 :: Int
test1 = nat2int test

multNat :: Nat -> Nat -> Nat
multNat n Zero = Zero
multNat n (Succ x) = addNat n (multNat n x)

test2 :: Int
test2 = nat2int $ multNat three two

-- multNat (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- addNat (Succ (Succ (Succ Zero))) (multNat (Succ (Succ (Succ Zero))) (Succ Zero))
-- addNat (Succ (Succ (Succ Zero))) (addNat (Succ (Succ (Succ Zero))) (multNat (Succ (Succ (Succ Zero))) Zero))
-- addNat (Succ (Succ (Succ Zero))) (addNat (Succ (Succ (Succ Zero))) Zero)
-- addNat (Succ (Succ (Succ Zero))) (Succ (addNat (Succ (Succ Zero)) Zero)) -- Succ (addNat m n)
-- addNat (Succ (Succ (Succ Zero))) (Succ (Succ (addNat (Succ Zero) Zero))) -- Succ (addNat m n)
-- addNat (Succ (Succ (Succ Zero))) (Succ (Succ (Succ (addNat Zero Zero)))) -- Succ (addNat m n)
-- addNat (Succ (Succ (Succ Zero))) (Succ (Succ (Succ (addNat Zero Zero)))) -- addNat Zero n = n
-- addNat (Succ (Succ (Succ Zero))) (Succ (Succ (Succ Zero))) -- addNat (Succ m) n = Succ (addNat m n)
-- Succ (addNat (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))) -- addNat (Succ m) n = Succ (addNat m n)
-- Succ (Succ (addNat (Succ Zero) (Succ (Succ (Succ Zero))))) -- addNat (Succ m) n = Succ (addNat m n)
-- Succ (Succ (addNat (Succ Zero) (Succ (Succ (Succ Zero))))) -- addNat (Succ m) n = Succ (addNat m n)
-- Succ (Succ (Succ (addNat Zero (Succ (Succ (Succ Zero)))))) -- addNat Zero n = n
-- Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

-- 2. Although not included in appendix B, the standard prelude defines
-- data Ordering = LT | EQ | GT
-- together with a function compare :: Ord a => a -> a -> Ordering
-- that decides if one value in an ordered type is less than (LT),
-- equal to (EQ), or greater than (GT) another value.
-- Using this function, redefine the function
-- occurs :: Ord a => a -> Tree a -> Bool for search trees.
-- Why is this new definition more efficient than the original version?
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs0 :: Eq a => a -> Tree a -> Bool
occurs0 x (Leaf y) = x == y
occurs0 x (Node l y r) = x == y || occurs0 x l || occurs0 x r

--   3      7
--  / \    / \
-- 1   4  6   9
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf a) = x == a
occurs x (Node l y r) = case x `compare` y of
  LT -> occurs x l
  EQ -> True
  GT -> occurs x r

-- 3. Consider the following type of binary trees:
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)

-- Let us say that such a tree is balanced
-- if the number of leaves in the left and right subtree of every node differs by
-- at most one, with leaves themselves being trivially balanced.
-- Define a function balanced :: Tree a -> Bool that
-- decides if a binary tree is balanced or not.
-- Hint: first define a function that returns the number of leaves in a tree.

leaves :: Tree' a -> Int
leaves (Leaf' _) = 1
leaves (Node' l r) = leaves l + leaves r

balanced :: Tree' a -> Bool
balanced (Leaf' x) = True
balanced (Node' l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

-- 4. Define a function balance :: [a] -> Tree a that
-- converts a non-empty list into a balanced tree.
-- Hint: first define a function that splits a list into two halves
-- whose length differs by at most one.
halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree' a
balance (x : []) = Leaf' x
balance list =
  let (l, r) = halves list
   in Node' (balance l) (balance r)

-- 5. Given the type declaration
data Expr = Val Int | Add Expr Expr

-- define a higher-order function
-- folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
-- such that folde f g replaces
-- each Val constructor in an expression by the function f, and
-- each Add constructor by the function g.
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val i) = f i
folde f g (Add a b) = g (folde f g a) (folde f g b)

-- 6. Using folde, define a function eval :: Expr -> Int
-- that evaluates an expression to an integer value, and 
-- a function size :: Expr -> Int that calculates the number of values in an expression.
eval :: Expr -> Int
eval (Val i) = i
eval (Add a b) = eval a + eval b

size :: Expr -> Int
size (Val i) = 1
size (Add a b) = size a + size b

-- 7. Complete the following instance declarations:
class Eq0 a where
    (===), (/==) :: a -> a -> Bool

    x /== y = not (x === y)

instance Eq0 Bool where
    False === False = True
    True  === True  = True
    _     === _     = False

instance Eq a => Eq0 (Maybe a) where
    (===) :: Maybe a -> Maybe a -> Bool
    (===) (Just a) (Just b) = a == b
    (===) (Just _) Nothing  = False
    (===) Nothing  (Just _) = False
    (===) Nothing  Nothing  = True

instance Eq a => Eq0 [a] where
  (===) :: [a] -> [a] -> Bool
  -- (===) (x:xs) (y:ys) = all const True (x == y :  xs === ys)
  (===) []     (y:ys) = False
  (===) (x:xs) []     = False
  (===) []     []     = True
-- ...

-- 8. Extend the tautology checker to support the use of
-- logical disjunction and equivalence (⇔) in propositions.
data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Assoc k v = [(k, v)]

type Subst = Assoc Char Bool

eval' :: Subst -> Prop -> Bool
eval' _ (Const b) = b
-- eval' s (Var x)     = find x s
eval' s (Not p) = not (eval' s p)
eval' s (And p q) = eval' s p && eval' s q
eval' s (Imply p q) = eval' s p <= eval' s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = []
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

-- bools' :: Int -> [[Bool]]
-- bools' n = map (reverse . map conv . make n . int2bin) range
--           where
--               range     = [0..(2^n)-1]
--               make n bs = take n (bs ++ repeat 0)
--               conv 0    = False
--               conv 1    = True

-- 9. Extend the abstract machine to support the use of multiplication.
data Expr0 = Val0 Int | Add0 Expr0 Expr0 
-- | Mul  Expr0 Expr0

value :: Expr0 -> Int
value (Val0 n) = n
value (Add0 x y) = value x + value y
-- value (Mul x y) = value x * value y


-- For example, the expression (2 + 3) + 4 is evaluated as follows:
example :: Int
example = value (Add0 (Add0 (Val0 2) (Val0 3)) (Val0 4))

-- example1 :: Int
-- example1 = value (Mul (Add0 (Val0 2) (Val0 3)) (Val0 4))

type Cont = [Op]

data Op = EVAL Expr0 | ADD Int
-- | MUL Int

eval'' :: Expr0 -> Cont -> Int
eval'' (Val0 n) c = exec c n
eval'' (Add0 x y) c = eval'' x (EVAL y : c)
-- eval'' (Mul  x y) c = eval'' x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval'' y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)
-- exec (MUL n : c) m = exec c (n * m)

value' :: Expr0 -> Int
value' e = eval'' e []

-- here is how it evaluates (2 + 3) + 4:
example2 :: Int
example2 = value' (Add0 (Add0 (Val0 2) (Val0 3)) (Val0 4))

-- (2 + 3) * 4
-- example3 :: Int
-- example3 = value' (Mul (Add0 (Val0 2) (Val0 3)) (Val0 4))

-- value' (Mul (Add0 (Val0 2) (Val0 3)) (Val0 4)) -- eval'' e []
-- eval'' (Mul (Add0 (Val0 2) (Val0 3)) (Val0 4)) [] -- eval'' x (EVAL y : c)
-- eval'' (Add0 (Val0 2) (Val0 3)) (EVAL (Val0 4) : [])
-- eval'' (Add0 (Val0 2) (Val0 3)) [EVAL (Val0 4)]
