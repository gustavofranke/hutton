module Main where

import Test.DocTest

main :: IO ()
main = doctest [
      "src/Part1/Ch01"
    , "src/Part1/Ch02"
    , "src/Part1/Ch03"
    , "src/Part1/Ch04"
    , "src/Part1/Ch05"
    , "src/Part1/Ch06"
    , "src/Part1/Ch07"
    , "src/Part1/Ch08"
    , "src/Part1/Ch09/Countdown.hs"
    , "src/Part2/Ch10/Life/Life.hs"
    , "src/Part2/Ch11/TicTacToe.hs"
    , "src/Part2/Ch12"
    , "src/Part2/Ch13"
    , "src/Part2/Ch14"
    , "src/Part2/Ch15"
    ]
