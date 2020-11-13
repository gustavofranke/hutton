{-# LANGUAGE InstanceSigs #-}

module Part2.Ch14.Exercises where

-- 14.5 Exercises
-- 1. Complete the following instance declaration from Data.Monoid
-- to make a pair type into a monoid
-- provided the two component type monoid provided the two component types are monoids:
class Monoid0 a where
  mempty0 :: a
  mappend0 :: a -> a -> a

instance (Monoid0 a, Monoid0 b) => Monoid0 (a, b) where
  mempty0 :: (a, b)
  mempty0 = (mempty0, mempty0)
  mappend0 :: (a, b) -> (a, b) -> (a, b)
  (x1, y1) `mappend0` (x2, y2) = (x1 `mappend0` x2, y1 `mappend0` y2)

-- 2. In a similar manner, show how a function type a -> b can be made into
-- a monoid provided that the result type b is a monoid.
instance Monoid0 (a -> b) where
  mempty0 :: (a -> b)
  mempty0 = undefined
  mappend0 :: (a -> b) -> (a -> b) -> (a -> b)
  f `mappend0` g = undefined

-- 3. Show how the Maybe type can be made foldable and traversable,
-- by giving explicit definitions for fold, foldMap, foldr, foldl and traverse.

class Foldable0 f where
  fold0 :: Monoid a => f a -> a
  foldMap0 :: Monoid b => (a -> b) -> f a -> b
  foldr0 :: (a -> b -> b) -> b -> f a -> b
  foldl0 :: (a -> b -> a) -> a -> f b -> a

class Traversable0 t where
  traverse0 :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Foldable0 Maybe where
  fold0 :: Monoid a => Maybe a -> a
  fold0 = undefined

  foldMap0 :: Monoid b => (a -> b) -> Maybe a -> b
  foldMap0 = undefined

  foldr0 :: (a -> b -> b) -> b -> Maybe a -> b
  foldr0 = undefined

  foldl0 :: (a -> b -> a) -> a -> Maybe b -> a
  foldl0 = undefined

instance Traversable0 Maybe where
  traverse0 :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse0 = undefined

-- 4. In a similar manner, show how the following type of binary trees
-- with data in their nodes can be made into a foldable and traversable type:
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Foldable Tree where
  foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf = undefined
  foldMap g (Node tx a ty) = undefined

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f y Leaf = undefined
  foldr f y (Node tx a ty) = undefined

  foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl f x Leaf = undefined
  foldl f x (Node tx a ty) = undefined
  
--   fold :: Monoid a => Tree a -> a
--   fold = undefined

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf = Leaf
    fmap g (Node tx a ty) = Node (fmap g tx) (g a) (fmap g ty)

instance Traversable Tree where
    traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse _ Leaf = undefined
    traverse g (Node tx a ty) = undefined

-- 5. Using foldMap, define a generic version of the higher-order
-- function filter on lists that can be used with any foldable type:
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p ta = undefined --foldMap _ _-- p ta
    -- undefined
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m