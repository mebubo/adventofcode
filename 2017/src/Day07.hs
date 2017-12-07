module Day07 where

import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Prim (parse)
import Text.Parsec.Char (anyChar, char, digit)
import Text.ParserCombinators.Parsec (many, many1, sepBy, eof, (<|>))
import Text.ParserCombinators.Parsec.Char (alphaNum)
import Data.List (find)

type Input = [Program]

type Name = String
type Weight = Int

data Program = Program Name Weight [Name] deriving Show

name :: Parser Name
name = many1 alphaNum

weight :: Parser Int
weight = do
    char '('
    i <- read <$> many1 digit
    char ')'
    return i

arrow :: Parser ()
arrow = do
    char '-'
    char '>'
    pure ()

above :: Parser [Name]
above = do
    whitespace
    arrow
    whitespace
    name `sepBy` (char ',' >> char ' ')

whitespace :: Parser Char
whitespace = char ' '

empty :: Parser [Name]
empty = pure []

program :: Parser Program
program = do
    n <- name
    whitespace
    w <- weight
    a <- above <|> empty
    char '\n'
    return $ Program n w a

programs :: Parser [Program]
programs = do
    result <- many program
    eof
    return result

readInput :: IO Input
readInput = do
    Right x <- parseFromFile programs "input/07.input"
    return x

aboves :: Input -> [Name]
aboves = (=<<) f
    where
        f (Program _ _ x) = x

belows :: Input -> [Name]
belows = (<$>) f
    where
        f (Program x _ _) = x

bottom :: Input -> Maybe Name
bottom i = find (not . (`elem` as)) bs
    where
        as = aboves i
        bs = belows i

main :: IO ()
main = do
    i <- readInput
    print $ bottom i
