module Part2.Ch15.Lazy where
 
inc :: Int -> Int
inc n = n + 1

a = inc (2*3) -- applying *
b = inc (6) -- applying inc
c = 7

d = inc (2*3) -- applying inc
e = (2*3) + 1 -- applying *
f = (6) + 1 -- applying +
g = 7

-- 15.2 Evaluation strategies
mult :: (Int,Int) -> Int
mult (x,y) = x * y

h = mult (1+2,2+3) -- this expression contains three redexes
i = mult (3,  2+3)
j = mult (1+2,5)
k = (1+2) * (2+3)

-- evaluated using innermost evaluation
l = mult (1+2,2+3) -- applying the first +
m = mult (3,2+3) -- applying +
n = mult (3,5) -- applying mult
o = 3 * 5 -- applying *
p = 15

-- evaluated using outerrmost evaluation
q = mult (1+2,2+3) -- applying mult
r = (1+2) * (2+3) -- applying the first +
s = (3) * (2+3) -- applying +
t = (3) * (5) -- applying *
u = 15

mult0 :: Int -> Int -> Int
mult0 x = \y -> x * y

v = mult0 (1+2) (2+3) -- applying the first +
w = mult0 (3) (2+3) -- applying mult0
y = (\y -> 3 * y) (2+3)-- applying +
z = (\y -> 3 * y) 5 -- applying the lambda
a0 = 3 * 5 -- applying *
b0 = 15

-- 15.3 Termination
inf :: Int
inf = 1 + inf

c0 = inf -- applying inf
d0 = 1 + inf -- applying inf
e0 = 1 + (1 + inf) -- applying inf
f0 = 1 + (1 + (1 + inf)) -- applying inf

g0 = fst (0,inf) -- applying inf
h0 = fst (0,1 + inf) -- applying inf
i0 = fst (0,1 + (1 + inf)) -- applying inf
j0 = fst (0,1 + (1 + (1 + inf))) -- applying inf

k0 = fst (0, inf) -- applying fst
l0 = 0

-- 15.4 Number of reductions
-- Now consider the following definition:
square :: Int -> Int
square n = n * n

-- For example, using call-by-value evaluation, we have:
m0 = square (1+2) -- applying +
n0 = square 3 -- applying square
o0 = 3 * 3 -- applying *
p0 = 9

-- 15.5 Infinite structures
ones :: [Int]
ones = 1 : ones

q0 = ones -- applying ones
r0 = 1 : ones -- applying ones
s0 = 1 : (1 : ones) -- applying ones
t0 = 1 : (1 : (1 : ones)) -- applying ones

u0 = head ones -- applying ones
v0 = head (1 : ones) -- applying ones
w0 = head (1 : (1 : ones)) -- applying ones
y0 = head (1 : (1 : (1 : ones))) -- applying ones

z0 = head ones -- applying ones
a1 = head (1 : ones) -- applying head
b1 = 1

-- 15.6 Modular programming
c1 = take 3 ones -- applying ones
d1 = take 3 (1 : ones) -- applying take n (x:xs) = x : take (n-1) xs
e1 = 1 : take 2 ones -- applying ones
f1 = 1 : take 2 (1 : ones) -- applying take n (x:xs) = x : take (n-1) xs
g1 = 1 : 1 : take 1 ones -- applying ones
h1 = 1 : 1 : take 1 (1 : ones) -- applying take n (x:xs) = x : take (n-1) xs
i1 = 1 : 1 : 1 : take 0 ones -- applying take 0 _ = []
j1 = 1 : 1 : 1 : [] -- applying take 0 _ = []
k1 = [1,1,1]

primes :: [Int]
primes = sieve [2..]
sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs
                            , x `mod` p /= 0]

l1 = primes
m1 = take 10 primes
n1 = takeWhile (< 10) primes

-- 15.7 Strict application
o1 = square $! (1+2) -- applying +
p1 = square $! (3) -- applying $!
q1 = square 3 -- applying square
r1 = 3 * 3 -- applying *
s1 = 9 -- applying *