{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Part2.Ch12.Exercises where

import GHC.Base -- (Functor, fmap, Applicative, pure, (<*>), Int, undefined, (.), const)
import GHC.Show
import GHC.List

-- 12.5 Exercises
-- 1. Define an instance of the Functor class for the following type of
-- binary trees that have data in their nodes:
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf = Leaf
    fmap f (Node t1 x t2) = Node (fmap f t1) (f x) (fmap f t2)

-- 2. Complete the following instance declaration to make the
-- partially-applied function type (a ->) into a functor:
class Funktor f where
    fmap0 :: (a -> b) -> f a -> f b

instance Funktor ((->) a) where
    fmap0 :: (a1 -> b) -> (a -> a1) -> a -> b
    -- fmap0 f paf = f . paf
    fmap0 = (.)

-- Hint: first write down the type of fmap,
-- and then think if you already know a library function that has this type.

-- 3. Define an instance of the Applicative class for the type (a ->).
-- If you are familiar with combinatory logic,
-- you might recognise pure and <*> for this type as being
-- the well-known K and S combinators.
class Funktor f => Applikative f where
    pure0 :: a -> f a
    (<**>) :: f (a -> b) -> f a -> f b
instance Applikative ((->) a) where
    pure0 :: x -> (a -> x)
    -- pure0 x1 = const x1
    pure0 = const
    (<**>) :: (a -> a1 -> b) -> (a -> a1) -> a -> b
    (<**>) f g x = f x (g x)

-- 4. There may be more than one way to make a parameterised type into
-- an applicative functor.
-- For example, the library Control.Applicative provides an alternative
-- 'zippy' instance for lists,
-- in which the function pure makes an infinite list of copies of its argument,
-- and the operator <*> applies each argument function
-- to the corresponding argument value at the same position.

-- Complete the following declarations that implement this idea:

newtype ZipList a = Z [a] deriving Show
instance Functor ZipList where
  fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  pure :: a -> ZipList a
  pure x = Z (repeat x)
  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | g <- gs, x <- xs]

-- The ZipList wrapper around the list type is required because
-- each type can only have at most one instance declaration for a given class.

-- 5. Work out the types for the variables in the four applicative laws.
-- pure id <*> x   = x
-- pure (g x)      = pure g <*> pure x
-- x <*> pure y    = pure (\g -> g y) <*> x
-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

-- 6. Define an instance of the Monad class for the type (a ->).
class Applikative m => Mona m where
  (>>>=) :: m a -> (a -> m b) -> m b
  retu :: a -> m a
  retu = pure0

instance Mona ((->) a) where
  (>>>=) :: (a -> a1) -> (a1 -> a -> b) -> a -> b
  (>>>=) f g x = ((g . f) x) x

-- 7. Given the following type of expressions
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show
-- that contain variables of some type a,
-- show how to make this type into instances of
-- the Functor, Applicative and Monad classes.
-- With the aid of an example,
-- explain what the >>= operator for this type does.
instance Functor Expr where
  fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var x) = Var (f x)
  fmap f (Val x) = undefined
  fmap f (Add x y) = undefined

instance Applicative Expr where
  pure :: a -> Expr a
  pure = Var
  (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  (<*>) = undefined

instance Monad Expr where
  (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (>>=) = undefined

-- 8. Rather than making a parameterised type into instances of
-- the Functor, Applicative and Monad classes in this order,
-- in practice it is sometimes simpler to define
-- the functor and applicative instances in terms of the monad instance,
-- relying on the fact that the order in which declarations are made
-- is not important in Haskell.
-- Complete the missing parts in the following declarations
-- for the ST type using the do notation.
type State = Int

newtype ST a = S(State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do
      x <- st
      return (g x)

instance Applicative ST where
  pure :: a -> ST a
  pure x = S (\s -> (x, s))
  (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do
      x <- stx
      f <- stf
      return (f x)

instance Monad ST where
  (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f =
    S
      ( \s ->
          let (x, s') = app st s in app (f x) s'
      )

