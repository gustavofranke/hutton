module Part1.Ch08.Tautology where

import Part1.Ch08.Ch08
import Part1.Ch07.Ch07
import Part1.Ch07.Voting

-- 8.6 Tautology checker
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

-- A & -A
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

-- (A & B) => A
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

-- A => (A & B)
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

--
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find0 x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

test13 = bools 3
-- [[False,False,False],
--  [False,False,True],
--  [False,True,False],
--  [False,True,True],
--  [True,False,False],
--  [True,False,True],
--  [True,True,False],
--  [True,True,True]]

bools0 :: Int -> [[Bool]]
bools0 n = map (reverse . map conv . make n . int2bin) range
          where
              range     = [0..(2^n)-1]
              make n bs = take n (bs ++ repeat 0)
              conv 0    = False
              conv 1    = True

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
            where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where vs = rmdups (vars p)

test14 = substs p2 -- [('A',False),('B',False)],[('A',False),('B',True)],[('A',True),('B',False)],[('A',True),('B',True)]]

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

test15 = isTaut p1 -- False
test16 = isTaut p2 -- True
test17 = isTaut p3 -- False
test18 = isTaut p4 -- True
