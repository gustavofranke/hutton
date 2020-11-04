module Part2.Ch17.Defunc where

data Expr = Val Int | Add Expr Expr
type Stack = [Int]
push :: Int -> Stack -> Stack
push n s = n : s
add :: Stack -> Stack
add (m : n : s) = n+m : s 

-- 17.5 Defunctionalising
-- Rather than using functions of
type Cont = Stack -> Stack 

haltC :: Cont
haltC = id

pushC :: Int -> Cont -> Cont
pushC n c = c . push n

addC :: Cont -> Cont
addC c = c . add

-- our evaluators can now be rewritten 
eval' :: Expr -> Cont
eval' e = eval'' e haltC

eval'' :: Expr -> Cont -> Cont
eval'' (Val n) c = pushC n c
eval'' (Add x y) c = eval'' x (eval'' y (addC c))

data Code = HALT | PUSH Int Code | ADD Code deriving Show

-- the expression 1 + 2
a = PUSH 1 (PUSH 2 (ADD HALT)) -- PUSH 1 (PUSH 2 (ADD HALT))

exec0 :: Code -> Cont
exec0 HALT = haltC
exec0 (PUSH n c) = pushC n (exec0 c)
exec0 (ADD c) = addC (exec0 c)

s = []
n = 5
c = HALT

-- HALT case:
b = exec0 HALT s -- applying exec0
c' = haltC s -- applying haltC
d = id s -- applying id
e' = s -- applying id

-- PUSH case:
f = exec0 (PUSH n c) s -- applying exec0
g = pushC n (exec0 c) s -- applying pushC
h = (exec0 c . push n) s -- applying .
i = exec0 c (push n s) -- applying push
j = exec0 c (n : s)

-- ADD case:
k = exec0 (ADD c) s -- applying exec0
l = addC (exec0 c) s -- applying addC
m = (exec0 c . add) s -- applying .
n' = exec0 c (add s) -- assume s of the form m : n : s
--  exec0 c (add m : n : s') -- assume s of the form m : n : s'
--  exec0 c (n+m : s') -- assume s of the form m : n : s'

-- In conclusion, we have calculated the following definition
exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (m : n : s) = exec c (n+m : s)

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))

--  17.6 Combining the steps

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

-- base case
o = exec (comp' (Val n) c) s -- specification of comp'
p = exec c (eval (Val n) : s) -- applying eval
r = exec c (n : s)

s' = exec c (n : s) -- unapplying exec
t = exec (PUSH n c) s

x = Val 1
y = Val 2
-- inductive case
u = exec (comp' (Add x y) c) s -- specification of comp'
v = exec c (eval (Add x y) : s) -- applying eval
v' = exec c (eval x + eval y : s) -- applying eval

w = exec c (eval x + eval y : s) -- unapplying exec
y' = exec (ADD c) (eval y : eval x : s) -- induction hypothesis for y
z = exec (comp' y (ADD c)) (eval x : s) -- induction hypothesis for x
a0 = exec (comp' x (comp' y (ADD c))) s

e = Val 5
b0 = exec (comp e) s -- specification of comp
c0 = eval e : s -- define: exec HALT s = s
d0 = exec HALT (eval e : s) -- specification of comp'
e0 = exec (comp' e HALT) s

-- In conclusion, we have calculated the following definitions:
compF :: Expr -> Code
compF e = comp'F e HALT

comp'F :: Expr -> Code -> Code
comp'F (Val n) c = PUSH n c
comp'F (Add x y) c = comp'F x (comp'F y (ADD c))

execF :: Code -> Stack -> Stack
execF HALT s = s
execF (PUSH n c) s = execF c (n : s)
execF (ADD c) (m : n : s) = execF c (n+m : s)
