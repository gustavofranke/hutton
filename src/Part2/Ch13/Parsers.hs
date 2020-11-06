{-# LANGUAGE InstanceSigs #-}

module Part2.Ch13.Parsers where

import Control.Applicative
import Data.Char

-- type Parser0 = String -> Tree -- takes a string and produces a tree
-- type Parser1 = String -> (Tree,String) -- also return any unconsumed part of the argument string
-- type Parser2 = StPArsersring -> [(Tree,String)] -- return a list of results, with the convention that the empty list denotes failure, and a singleton list denotes success

-- type Parser3 = String -> [(a,String)] -- different parsers will likely return different kinds of trees
-- Parser is similar to the type State -> (a,State) for state transformers
-- parser also has the possibility to fail by returning a list of results,
-- whereas a state transformer always returns a single result

newtype Parser a = P (String -> [(a,String)]) -- To allow the Parser type to be made into instances of classes
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

-- fails if the input string is empty
item :: Parser Char
item = P (\inp -> case inp of
            [] -> []
            (x:xs) -> [(x,xs)])

test1 = parse item ""    -- []
test2 = parse item "abc" -- [('a',"bc")]

-- 13.4 Sequencing parsers
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
            [] -> []
            [(v, out)] -> [(g v,out)])

test3 = parse (fmap toUpper item) "abc" -- [('A',"bc")]
test4 = parse (fmap toUpper item) "" -- []

instance Applicative Parser where
    pure :: a -> Parser a
    pure v = P (\inp -> [(v,inp)])
    
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
        [] -> []
        [(g,out)] -> parse (fmap g px) out)

test5 = parse (pure 1) "abc" -- [(1,"abc")]

-- a parser that consumes three characters,
-- discards the second,
-- and returns the first and third as a pair
-- can now be defined in applicative style:
three :: Parser (Char,Char)
three = pure g <*> item <*> item <*> item
         where g x y z = (x,z)

test6 = parse three "abcdef" -- [(('a','c'),"def")]
test7 = parse three "ab" -- []

instance Monad Parser where -- parse :: Parser a -> String -> [(a,String)]
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
        [] -> []
        [(v,out)] -> parse (f v) out)
 
threeM :: Parser (Char,Char)
threeM = do x <- item
            item
            z <- item
            return (x,z)

test8 = parse threeM "abcdef" -- [(('a','c'),"def")]

-- empty is the parser that always fails regardless of the input string,
-- and <|> is a choice operator that returns the 
-- result of the first parser if it succeeds on the input,
-- and applies the second parser to the same input otherwise”

instance Alternative Parser where
    empty :: Parser a
    empty = P (\inp -> [])
    (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
              [] -> parse q inp
              [(v,out)] -> [(v,out)])

test9  = parse empty "abc" -- []
test10 = parse ( item <|> return 'd') "abc" -- [('a',"bc")]
test11 = parse (empty <|> return 'd') "abc" -- [('d',"abc")]

-- 13.6 Derived primitives
-- We now have three basic parsers:
-- item consumes a single character if the input string is non-empty,
-- return v always succeeds with the result value v,
-- and empty always fails.
-- In combination with sequencing and choice,
-- these primitives can be used to define a number of other useful parsers.

-- parser sat p, for single characters that satisfy the predicate p
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

-- Using sat and appropriate predicates from the library Data.Char,
-- we can now define parsers for single digits,
digit :: Parser Char
digit = sat isDigit

-- lower-case letters,
lower :: Parser Char
lower = sat isLower

-- upper-case letters,
upper :: Parser Char
upper = sat isUpper

-- arbitrary letters,
letter :: Parser Char
letter = sat isAlpha

-- alphanumeric characters,
alphanum :: Parser Char
alphanum = sat isAlphaNum

-- and specific characters
char :: Char -> Parser Char
char x = sat (== x)

test12 = parse (char 'a') "abc" -- [('a',"bc")]

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

test13 = parse (string "abc") "abcdef" -- [("abc","def")]

-- string only succeeds if the entire target string is consumed from the input
test14 = parse (string "abc") "ab1234" -- []

-- many p and some p (definitions are already provided in the Alternative class),
-- apply a parser p as many times as possible until it fails,
-- with the result values from each successful application of p being returned in a list.
-- The difference between these two repetition primitives is that
-- many permits zero or more applications of p,
-- whereas some requires at least one successful application
test15 = parse (many digit) "123abc" -- [("123","abc")]
test16 = parse (many digit) "abc" -- [("","abc")]
test17 = parse (some digit) "abc" -- []

-- Using many and some, we can now define parsers for identifiers (variable names)
-- comprising a lower-case letter followed by zero or more alphanumeric characters,
-- natural numbers comprising one or more digits,
-- and spacing comprising zero or more space, tab, and newline character
ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

-- nat converts the number that was read into an integer 
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

-- space returns the empty tuple () as a dummy result value
space :: Parser ()
space = do many (sat isSpace)
           return ()

test18 = parse ident "abc def" -- [("abc"," def")]
test19 = parse nat "123 def" -- [(123," def")]
test20 = parse space " abc" -- [((),"abc")]

-- using nat it is now straightforward to define a parser for integer values
int :: Parser Int
int = do char '-'
         n <- nat
         return (-n) <|> nat

test21 = parse int "-123 abc" -- [(-123," abc")]

-- 13.7 Handling spacing
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

-- we can now define parsers that ignore spacing around identifiers,
-- natural numbers,
-- integers and special symbols
identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- a parser for a non-empty list of natural numbers that ignores spacing around tokens
nats :: Parser [Int]
nats = do symbol "["
          n  <- natural
          ns <- many (do symbol ","; natural)
          symbol "]"
          return (n:ns)

test22 = parse nats " [1, 2, 3] " -- [([1,2,3],"")]
test23 = parse nats "[1, 2,] " -- []

-- 13.8 Arithmetic expressions

---- a grammar for our language of arithmetic expressions 
-- expr ::= expr + expr | expr * expr | (expr) | nat
-- nat ::= 0 | 1 | 2 | ...

---- a separate rule for each level of priority, with addition at the lowest level of priority
-- expr ::= expr + expr | term
-- term ::= term * term | factor
-- factor ::= (expr) | nat
-- nat ::= 0 | 1 | 2 | ...

---- addition and multiplication associate to the right, 
---- the rules for addition and multiplication are now recursive in 
---- their right argument only, rather than in both arguments
-- expr ::= term + expr | term
-- term ::= factor * term | factor
-- factor ::= (expr) | nat
-- nat ::= 0 | 1 | 2 | ...

---- final grammar
-- expr ::= term (+ expr | ϵ)
-- term ::= factor (* term | ϵ)
-- factor ::= (expr) | nat
-- nat ::= 0 | 1 | 2 | ...

-- translate this grammar directly into a parser for expressions,
-- by simply rewriting the rules using the parsing primitives we have introduced.
-- Sequencing in the grammar is translated into the do notation,
-- choice | is translated into the <|> operator,
-- the empty string ϵ becomes the empty parser,
-- special symbols such as + and * are handled using the symbol function,
-- and natural numbers are parsed using the natural primitive
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
            <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
            <|> return f
        
factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
           <|> natural

eval0 :: String -> Int
eval0 xs = case (parse expr xs) of
            [(n, [])] -> n 
            [(_,out)] -> error ("Unused input " ++ out)
            [] -> error "Invalid input"

test24 = eval0 "2*3+4" -- 10
test25 = eval0 "2*(3+4)" -- 14
-- test26 = eval0 "2*3^4" -- *** Exception: Unused input ^4
-- test27 = eval0 "one plus two" -- *** Exception: Invalid input
