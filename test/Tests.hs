module Main where

import Test.DocTest

main :: IO ()
main = doctest [
      "src/Part1/Ch01"
    , "src/Part1/Ch02"
    , "src/Part1/Ch05"
    , "src/Part1/Ch06"
    ]
