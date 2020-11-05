{-# LANGUAGE NoImplicitPrelude #-}

-- | 7.7 Voting algorithms
module Part1.Ch07.Voting where

import Data.List (filter, head, last, length, sort)
import Data.Tuple (snd)
import GHC.Base (Eq, Int, Ord, String, map, (.), (/=), (==))

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

-- |
-- >>> count1 "Red" votes
-- 2
count1 :: Eq a => a -> [a] -> Int
count1 x = length . filter (== x)

-- |
-- >>> rmdups votes
-- ["Red","Blue","Green"]
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : filter (/= x) (rmdups xs)

-- result :: Ord a => [a] -> [(Int,a)]
-- result vs = sort [(count v vs, v) | v <- rmdups vs]

-- |
-- >>> result votes
-- [(1,"Green"),(2,"Red"),(3,"Blue")]
result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count1 v vs, v) | v <- rmdups vs]

-- |
-- >>> winner votes
-- "Blue"
winner :: Ord a => [a] -> a
winner = snd . last . result

-- Alternative vote
ballots :: [[String]]
ballots =
  [ ["Red", "Green"],
    ["Blue"],
    ["Green", "Red", "Blue"],
    ["Blue", "Green", "Red"],
    ["Green"]
  ]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

-- |
-- >>> rank ballots
-- ["Red","Blue","Green"]
rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

-- |
-- >>> winner' ballots
-- "Green"
winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
  [c] -> c
  (c : cs) -> winner' (elim c bs)
