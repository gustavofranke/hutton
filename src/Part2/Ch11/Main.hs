module Main where

import TicTacToe
import System.IO

-- Human vs computer
main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O