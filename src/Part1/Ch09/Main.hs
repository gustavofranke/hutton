module Main where

import Countdown

main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)