module Part2.Ch10.Exercises where

import System.IO
import Data.Foldable

-- 10.10 Exercises
-- 1. Redefine putStr :: String -> IO () using a list comprehension and
-- the library function sequence_ :: [IO a] -> IO ().
putStr' :: String -> IO ()
putStr' str = sequence_ [putChar x | x <- str]

-- 2. Using recursion, define a version of
-- putBoard :: Board -> IO () that displays nim boards of any size,
-- rather than being specific to boards with just five rows of stars.
-- Hint: first define an auxiliary function that
-- takes the current row number as an additional argument.
type Board = [Int]
putRow :: Int -> Int -> IO ()
putRow row num =
  putStr (show row)
    *> putStr ": "
    *> putStrLn (concat (replicate num "* "))

putBoard0 :: Board -> IO ()
putBoard0 [a, b, c, d, e] =
  putRow 1 a
    *> putRow 2 b
    *> putRow 3 c
    *> putRow 4 d
    *> putRow 5 e

putBoard :: Board -> IO ()
-- putBoard b = foldr (*>) (pure ()) $ map putRowInB (withIndex b)
-- putBoard b = sequenceA_ $ map putRowInB (withIndex b)
putBoard b = traverse_ putRowInB (withIndex b)
  where
    withIndex board  = zip board [(1 :: Int) ..]
    putRowInB (r, i) = putRow i r

-- 3. In a similar manner to the first exercise,
-- redefine the generalised version of putBoard
-- using a list comprehension and sequence_.
putBoard' :: Board -> IO ()
putBoard' b = sequence_ [putChar x | x <- (show b)]

-- 4. Define an action adder :: IO () that reads a given number of integers from the keyboard,
-- one per line, and displays their sum. For example:
-- > adder
-- How many numbers? 5
-- 1
-- 3
-- 5
-- 7
-- 9
-- The total is 25
-- Hint: start by defining an auxiliary function that takes the current total
-- and how many numbers remain to be read as arguments.
-- You will also likely need to use the library functions read and show.
readInput :: String -> Int
readInput input = read input :: Int

aux :: Int -> Int -> IO Int
aux current remain =
  if remain <= 0
    then pure current
    else do
      input <- getLine
      aux (current + (readInput input)) (remain - 1)

adder :: IO ()
adder = do
  putStrLn "How many numbers?"
  input <- getLine
  res <- aux 0 (readInput input)
  putStrLn ("The total is " ++ show res)

-- 5. Redefine adder using the function
-- sequence :: [IO a] -> IO [a] that
-- performs a list of actions and returns a list of the resulting values.
adder2 :: IO ()
adder2 = do
  putStrLn "How many numbers?"
  input <- getLine
  let atts = read input :: Int
--   list  <- sequence $ map (\_ -> readInput <$> getLine) [atts, atts -1 .. 1]
  list  <- mapM (\_ -> readInput <$> getLine) [atts, atts -1 .. 1]
  let res = sum list
  putStrLn ("The total is " ++ show res)
--   where
--     func = readInput <$> getLine
    -- func = do
    --   input <- getLine
    --   return (readInput input)

-- 6. Using getCh, define an action
-- readLine :: IO String that behaves in the same way as getLine,
-- except that it also permits the delete key to be used to remove characters.
-- Hint: the delete character is '\DEL', and
-- the control character for moving the cursor back one space is '\b'.”
cls :: IO ()
cls = putStr "\ESC[2J"

getCh :: IO Char
getCh = do
         hSetEcho stdin False
         x <- getChar
         hSetEcho stdin True
         return x

readLine :: IO String
readLine = do
  char <- getCh
  case char of
    '\n' -> return ""
    '\DEL' ->
         do putChar '\b'
            -- putChar '\b'
            -- putChar '\b'
            l <- readLine
            return ( l)
    _ -> do l <- readLine
            return (char : l)
