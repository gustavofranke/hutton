module Part1.Ch08.Machine where

-- 8.7 Abstract machine
data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val n)= n
value (Add x y) = value x + value y

type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval0 :: Expr -> Cont -> Int
eval0 (Val n)   c = exec c n
eval0 (Add x y) c = eval0 x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval0 y (ADD n : c)
exec (ADD n : c)  m = exec c (n+m)

value0 :: Expr -> Int
value0 e = eval0 e []

test19 = eval0 (Add (Val 2) (Val 3)) [EVAL (Val 4)]
