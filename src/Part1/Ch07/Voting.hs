module Part1.Ch07.Voting where

import Data.List

-- 7.7 Voting algorithms
votes :: [String]
votes = ["Red","Blue","Green","Blue","Blue","Red"]

count1 :: Eq a => a -> [a] -> Int
count1 x = length . filter (== x)

test6 :: Int
test6 = count1 "Red" votes -- 2

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

test7 = rmdups votes -- ["Red","Blue","Green"]

-- result :: Ord a => [a] -> [(Int,a)]
-- result vs = sort [(count v vs, v) | v <- rmdups vs]

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count1 v vs, v) | v <- rmdups vs]

test8 = result votes -- [(1,"Green"),(2,"Red"),(3,"Blue")]

winner :: Ord a => [a] -> a
winner = snd . last . result

test9 = winner votes -- "Blue"

-- Alternative vote
ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green","Red","Blue"],
           ["Blue","Green","Red"],
           ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

test10 :: [String]
test10 = rank ballots -- ["Red","Blue","Green"]

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c] -> c
                (c:cs) -> winner' (elim c bs)

test11 :: String
test11 = winner' ballots
