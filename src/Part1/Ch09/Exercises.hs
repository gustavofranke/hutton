module Part1.Ch09.Exercises where

-- 9.11 Exercises
-- 1. Redefine the combinatorial function choices
-- using a list comprehension rather than using composition, concat and map.
subs :: [a] -> [[a]]
subs [] = [[]]
-- subs (x : xs) = yss ++ map (x :) yss
--   where yss = subs xs
-- subs (x : xs) = (subs xs) ++ map (x :) (subs xs)
subs (x : xs) = [sxs ++ xsx | sxs <- subs xs
                            , xsx <- [x : sxs]
                            ]

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
-- interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)
interleave x (y : ys) = [ yins | ints <- interleave x ys
                               , yins <- (x : y : ys) : [y : ints]
                               ]

perms :: [a] -> [[a]]
perms [] = [[]]
-- perms (x : xs) = concat (map (interleave x) (perms xs))
perms (x : xs) = [ ins | pes <- perms xs
                       , ins <- interleave x pes
                       ]

choices :: [a] -> [[a]]
-- choices = concat . map perms . subs
choices as = [res | sas <- subs as
                  , res <- perms sas
                  ]

-- 2. Define a recursive function isChoice :: Eq a => [a] -> [a] -> Bool that decides if
-- one list is chosen from another, without using the combinatorial functions perms and subs.
-- Hint: start by defining a function that removes the first occurrence of a value from a list.
removesFirst :: Eq a => a -> [a] -> [a]
removesFirst _ [] = []
-- removesFirst n (x:xs) = if n == x then xs else x : removesFirst n xs
removesFirst n (x : xs)
  | n == x = xs
  | otherwise = x : removesFirst n xs

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x : xs) ys = elem x ys && isChoice xs (removesFirst x ys)

-- 3. What effect would generalising the function split to also
-- return pairs containing the empty list have on the behaviour of solutions?
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

-- 4. Using the functions
-- choices :: [a] -> [[a]]
-- exprs :: [Int] -> [Expr] and
-- eval :: Expr -> [Int]
-- verify that there are 33,665,406 possible expressions over the numbers 1, 3, 7, 10, 25, 50,
-- and that only 4,672,540 of these expressions evaluate successfully.
data Op = Add | Sub | Mul | Div

ops :: [Op]
ops = [Add, Sub, Mul, Div]

data Expr = Val Int | App Op Expr Expr

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [ e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r
  ]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) =
  [ apply o x y | x <- eval l
                , y <- eval r
                , valid' o x y
                -- , valid o x y
  ]

-- verify   [1, 3, 7, 10, 25, 50]
verify :: [Int] -> Int
verify ns = length [e | ns' <- choices ns
                        , e <- exprs ns'
                        -- , eval e == [n]
                        ]



-- 5. Similarly, verify that the number of expressions that evaluate successfully increases to 10,839,369
-- if the numeric domain is generalised to arbitrary integers.
-- Hint: modify the definition of valid.
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

eval2 :: Expr -> [Int]
eval2 (Val n) = [n | n > 0]
eval2 (App o l r) =
  [ apply o x y | x <- eval2 l
                , y <- eval2 r
                -- , valid' o x y
                , valid o x y
  ]

-- 6. Modify the final program to:
-- a.allow the use of exponentiation in expressions;
-- b.produce the nearest solutions if no exact solution is possible;
-- c.order the solutions using a suitable measure of simplicity.
-- exp :: (Fractional b, Integral a) => a -> b
-- exp00 x = sum $ map (\n -> (x^n) / fromIntegral (product [1..n])) [1..100]

-- expG :: Fractional a => a -> a
-- expG x = sum [(x^n) / fromIntegral (product [1..n]) | n <- [1..100]]
-- exp x = let fact a = fromIntegral (product [1..a])
--         in sum [(x^n) / fact n | n <- [1..100]]
