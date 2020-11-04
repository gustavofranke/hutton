module Part2.Ch17.CalcComp where

data Expr = Val Int | Add Expr Expr

eval0 :: Expr -> Int
eval0 (Val n) = n
eval0 (Add x y ) = eval0 x + eval0 y

-- For example, the expression 1 + 2 can be evaluated as follows
a = eval0 (Add (Val 1) (Val 2)) -- applying eval
b = eval0 (Val 1) + eval0 (Val 2) -- applying the first eval
c = 1 + eval0 (Val 2) -- applying eval
d = 1 + 2 -- applying +
e = 3

-- 17.3 Adding a stack
type Stack = [Int]

-- In conclusion, we have calculated the following definition
eval'0 :: Expr -> Stack -> Stack
eval'0 (Val n) s = push n s
eval'0 (Add x y) s = add (eval'0 y (eval'0 x s))

-- where:
push :: Int -> Stack -> Stack
push n s = n : s

add :: Stack -> Stack
add (m : n : s) = n+m : s

-- property: eval' e s = eval e : s
-- substituting the empty stack s = [] into the equation eval' e s = eval e : s
eval :: Expr -> Int
eval e = head (eval'0 e [])

-- using this new definition evaluation of 1+2
-- now proceeds by pushing the two values onto the stack prior adding them together:
f = eval (Add (Val 1) (Val 2)) -- applying eval
g = head (eval'0 (Add (Val 1) (Val 2)) []) -- applying eval'
h = head (add (eval'0 (Val 2) (eval'0 (Val 1) []))) -- applying inner eval'
i = head (add (push 2 (push 1 []))) -- applying push
j = head (add (push 2 (1 : []))) -- applying push
k = head (add (2 : 1 : [])) -- applying add
l = head (1+2 : []) -- applying head
m = 3 -- applying head

-- 17.4 Adding a continuation
type Cont = Stack -> Stack
-- In conclusion, we have calculated the following definition:
eval'' :: Expr -> Cont -> Cont
eval'' (Val n) c s = c (push n s)
eval'' (Add x y) c s = eval'' x (eval'' y (c . add)) s

eval' :: Expr -> Cont
eval' e s = eval'' e id s

n = eval' (Add (Val 1) (Val 2)) [] -- applying eval'
o = eval'' (Add (Val 1) (Val 2)) id [] -- applying eval''
p = eval'' (Val 1) (eval'' (Val 2) (id . add)) [] -- applying outer eval''
q = (eval'' (Val 2) (id . add)) (push 1 []) -- applying eval''
r = (id . add) (push 2 (push 1 [])) -- applying .
s = id (add (push 2 (push 1 []))) -- applying push
t = id (add (2 : 1 : [])) -- applying add 
u = id (1+2 : []) -- applying id 
v = [3] -- applying id 