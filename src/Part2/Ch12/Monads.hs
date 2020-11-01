module Part2.Ch12.Monads where

-- 12 Monads and more
---------------------

-- 12.1 Functors

inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n + 1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns

map0 :: (a -> b) -> [a] -> [b]
map0 _ []     = []
map0 f (x:xs) = f x : map0 f xs

inc' = map0 (+1)
sqr' = map0 (^2)

-- mapping a function over each element of a data structure isn’t specific to the type of lists,
-- but can be abstracted further to a wide range of parameterised types.
-- The class of types that support such a mapping function are called functors
class Functor0 f where
    fmap0 :: (a -> b) -> f a -> f b

instance Functor0 [] where
    -- fmap0 :: (a -> b) -> [a] -> [b]
    fmap0 = map

data Maybe0 a = Nothing0 | Just0 a deriving Show

instance Functor0 Maybe0 where
    -- fmap0 :: (a -> b) -> Maybe0 a -> Maybe0 b
    fmap0 _ Nothing0  = Nothing0
    fmap0 g (Just0 x) = Just0 (g x)

test1 = fmap0 (+1) Nothing0 -- Nothing0
test2 = fmap0 (*2) (Just0 3) -- Just0 6
test3 = fmap0 not (Just0 False) -- Just0 True

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor0 Tree where
    -- fmap0 :: (a -> b) -> Tree a -> Tree b
    fmap0 g (Leaf x)   = Leaf (g x)
    fmap0 g (Node l r) = Node (fmap0 g l) (fmap0 g r)

test4 = fmap0 length (Leaf "abc") -- Leaf 3
test5 = fmap0 even (Node (Leaf 1) (Leaf 2)) -- Node (Leaf False) (Leaf True)

instance Functor0 IO where
    -- fmap0 :: (a -> b) -> IO a -> IO b
    fmap0 g mx = do x <- mx
                    return (g x)

-- test6 = fmap0 show (return True)

inc'' :: Functor0 f => f Int -> f Int
inc'' = fmap0 (+1)

test7 = inc'' (Just0 1) -- Just0 2
test8 = inc'' [1,2,3,4,5] -- [2,3,4,5,6]
test9 = inc'' (Node (Leaf 1) (Leaf 2)) -- Node (Leaf 2) (Leaf 3)

-- Functor laws
-- fmap id = id
-- fmap (g . h) = fmap g . fmap h

-- 12.2 Applicatives
-- using the idea of currying,
-- it turns out that a version of fmap for functions with
-- any desired number of arguments can be constructed in terms of
-- two basic functions with the following types:
-- pure :: a -> f a -- converts a value of type a into a structure of type f a
-- (<*>) :: f (a -> b) -> f a -> f b -- generalised form of function application for which
-- the argument function,
-- the argument value,
-- and the result value are all contained in f structures

-- A typical use of pure and <*> has the following form:
-- pure g <*> x1 <*> x2 <*> ... <*> xn

-- fmap0' :: a -> f a
-- fmap0' = pure

-- fmap1 :: (a -> b) -> f a -> f b
-- fmap1 g x = pure g <*> x

-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap2 g x y = pure g <*> x <*> y

-- fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- fmap3 g x y z = pure g <*> x <*> y <*> z

class Functor0 f => Applicative0 f where
    pure0 :: a -> f a
    (<**>) :: f (a -> b) -> f a -> f b

-- Examples
instance Applicative0 Maybe0 where
    -- pure0 :: a -> Maybe0 a
    pure0 = Just0
    -- (<**>) :: Maybe0 (a -> b) -> Maybe0 a -> Maybe0 b
    Nothing0 <**> _ = Nothing0
    (Just0 g) <**> mx = fmap0 g mx

test10 = pure0 (+1) <**> Just0 1 -- Just0 2
test11 = pure0 (+) <**> Just0 1 <**> Just0 2 -- Just0 3
test12 = pure0 (+) <**> Nothing0 <**> Just0 2 -- Nothing0

instance Applicative0 [] where
    -- pure0 :: a -> [a]
    pure0 x = [x]
    -- (<**>) :: [a -> b} -> [a] -> [b]
    gs <**> xs = [g x | g <- gs, x <- xs]

test13 = pure0 (+1) <**> [1,2,3] -- [2,3,4]
test14 = pure0 (+) <**> [1] <**> [2] -- [3]
test15 = pure0 (*) <**> [1,2] <**> [3,4] -- [3,4,6,8]

prods :: [Int] -> [Int] -> [Int]
prods xs ys = pure0 (*) <**> xs <**> ys

instance Applicative0 IO where
    -- pure0 :: a -> IO a
    pure0 = return
    -- (<**>) :: IO (a -> b) -> IO a -> IO b
    mg <**> mx = do g <- mg
                    x <- mx
                    return (g x)

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure0 (:) <**> getChar <**> getChars (n-1)

-- Effectful programming
getChars' :: Int -> IO String
getChars' n = sequenceA (replicate n getChar)

-- Applicative laws
-- pure id <*> x = x
-- pure (g x)    = pure g <*> pure x
-- x <*> pure y  = pure (\g -> g y) <*> x
-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

-- 12.3 Monads
data Expr = Val Int | Div Expr Expr

eval0 :: Expr -> Int
eval0 (Val n) = n
eval0 (Div x y) = eval0 x `div` eval0 y

test16 = eval0 (Div (Val 1) (Val 0)) -- *** Exception: divide by zero

safediv :: Int -> Int -> Maybe0 Int
safediv _ 0 = Nothing0
safediv n m = Just0 (n `div` m)

eval1 :: Expr -> Maybe0 Int
eval1 (Val n)   = Just0 n
eval1 (Div x y) = case eval1 x of
    Nothing0 -> Nothing0
    Just0 n  -> case eval1 y of
        Nothing0 -> Nothing0
        Just0 m -> safediv n m

test17 = eval1 (Div (Val 1) (Val 0)) -- Nothing

-- Maybe is applicative, redefine eval in applicative style, but this definition is not type correct
-- eval :: Expr -> Maybe Int
-- eval (Val n)   = pure n
-- eval (Div x y) = pure safediv <*> eval x <*> eval y

-- The key is to observe the common pattern that occurs twice in its definition,
-- namely performing a case analysis on a Maybe value,
-- mapping Nothing to itself and Just x to some result depending on x.
-- Abstracting out this pattern gives a new operator >>= that is defined as follows:
(>>>==) :: Maybe0 a -> (a -> Maybe0 b) -> Maybe0 b
mx >>>== f = case mx of
    Nothing0 -> Nothing0
    Just0 x -> f x

eval2 :: Expr -> Maybe0 Int
eval2 (Val n) = Just0 n
eval2 (Div x y) = eval2 x >>>== \n ->
                    eval2 y >>>== \m ->
                        safediv n m

test18 = eval2 (Div (Val 1) (Val 0)) -- Nothing0

-- eval :: Expr -> Maybe0 Int
-- eval (Val n) = Just0 n
-- eval (Div x y) = do n <- eval x
--                     m <- eval y
--                     safediv n m

class Applicative0 m => Monad0 m where
    return0 :: a -> m a
    (>>==) :: m a -> (a -> m b) -> m b
    return0 = pure0

-- Examples
instance Monad0 Maybe0 where
    -- (>>==) :: Maybe0 a -> (a -> Maybe0 b) -> Maybe0 b
    Nothing0 >>== _ = Nothing0
    (Just0 x) >>== f = f x

instance Monad0 [] where
    -- (>>==) :: [a] -> (a -> [b]) -> [b]
    xs >>== f = [ y | x <- xs,
                      y <- f x]

-- The state monad
type State = Int
type ST0 = State -> State
type ST1 a = State -> (a, State)
newtype ST a = S(State -> (a, State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor0 ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap0 g st = S (\s ->
         let (x,s') = app st s
         in (g x, s'))

instance Applicative0 ST where
    -- pure :: a -> ST a
    pure0 x = S (\s -> (x,s))
    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <**> stx = S (\s ->
         let (f,s')   = app stf s
             (x, s'') = app stx s'
         in (f x, s''))

instance Monad0 ST where
    -- (>>==) :: ST a -> (a -> ST b) -> ST b
    st >>== f = S (\s ->
        let (x,s')   = app st s
        in app (f x) s')

-- Relabelling trees
tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- Now consider the problem of defining a function
-- that relabels each leaf in such a tree with a unique or fresh integer. 
-- This can be implemented in a pure language such as Haskell
-- by taking the next fresh integer as an additional argument,
-- and returning the next fresh integer as an additional result

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n   = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
        where
            (l',n')   = rlabel l n
            (r', n'') = rlabel r n'

test19 = fst (rlabel tree 0) -- Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

fresh :: ST Int
fresh  = S (\n -> (n,n+1))

-- new version of the relabelling function, written in applicative style
-- alabel :: Tree a -> ST (Tree Int)
-- alabel (Leaf _)   = Leaf <$> fresh
-- alabel (Node l r) = Node <$> alabel l <**> alabel r

-- mlabel :: Tree a -> ST (Tree Int)
-- mlabel (Leaf _) = do n <- fresh
--                      return (Leaf n)
-- mlabel (Node l r) = do l' <- mlabel l
--                        r' <- mlabel r
--                        return (Node l' r')

-- Generic functions

-- Monad laws
-- return x >>= f   = f x
-- mx >= return     = mx
-- (mx >>= f) >>= g = mx >>= (\x -> (fx >>= g))
