module Part2.Ch16.Equational where

-- 16.7 Compiler correctness (extended example)

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]
type Code = [Op]

data Op = PUSH Int | ADD deriving Show

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n : c) s = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c (n+m : s)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

e = Add (Add (Val 2) (Val 3)) (Val 4)

test0 = eval e -- 9
test1 = comp e -- [PUSH 2,PUSH 3,ADD,PUSH 4,ADD]

test2 = exec (comp e) [] -- [9]

test3 = exec (comp e) [] == [eval e] -- True

s = []
n = 1 :: Int
x = Val 2
y = Val 3
-- Base case:
a = exec (comp (Val n)) s -- applying comp
b = exec ([PUSH n]) s -- applying exec
b' = exec ((PUSH n):[]) s -- applying exec
c' = exec [] (n : s) -- applying exec
d' = (n : s) -- applying exec
e' = (eval (Val n) : s) -- unapplying eval

-- Inductive case:
f = exec (comp (Add x y)) s -- applying comp
g = exec (comp x ++ comp y ++ [ADD]) s -- associativity of ++
h = exec (comp x ++ (comp y ++ [ADD])) s -- distributivity ... exec (c ++ d) s = exec d (exec c s) ... needs proof
i = (exec (comp y ++ [ADD]) (exec (comp x) s)) -- induction hypothesis for x
j =  exec (comp y ++ [ADD]) (eval x : s) -- distributivity again
k =  exec [ADD] (exec (comp y) (eval x : s)) -- induction hypothesis for y
l =  exec [ADD] (eval y : eval x : s) -- applying exec 
m =  (eval x + eval y) : s -- unapplying eval
n' = eval (Add x y) : s

---- exec (c ++ d) s = exec d (exec c s)
c = [ADD] -- (comp y ++ [ADD])
d = [ADD] -- (comp y ++ [ADD])

-- Base case:
o = exec ([] ++ d) s -- applying ++ 
p = exec (d) s
q = exec d (exec [] s)

-- Inductive case:
r = exec ((PUSH n : c) ++ d) s -- applying ++ 
t = exec (PUSH n : (c ++ d)) s -- applying exec
u = exec (c ++ d) (n : s) -- induction hypothesis
v = exec d (exec c (n : s)) -- unapplying exec
w = exec d (exec (PUSH n : c) s)

m' = 1 :: Int
n'' = 1 :: Int
s' = [1] :: [Int]
-- Inductive case:
y' = exec ((ADD : c) ++ d) s -- applying ++ 
z = exec (ADD : (c ++ d)) s -- assume s of the form m : n : s
a0 = exec (ADD : (c ++ d)) (m' : n'' : s') -- assume s of the form m : n : s'
b0 = exec (c ++ d) (n'' + m' : s') -- applying exec
c0 = exec d (exec c (n'' + m' : s')) -- induction hypothesis
d0 = exec d (exec (ADD : c) (m' : n'' : s')) -- unapplying exec

-- comp’ e c = comp e ++ c

-- By induction on e, we can construct the definition
comp' :: Expr -> Code -> Code
comp' (Val n) c= PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))

-- from which it follows that we can redefine
comp0 e = comp' e []
