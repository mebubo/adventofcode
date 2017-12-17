module Day09 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Tree (Tree(..), Forest, levels)
import Data.Maybe (catMaybes)

type Parser = Parsec () String

garbage :: Parser ()
garbage = do
    char '<'
    many $ escaped <|> nonClose
    char '>'
    return ()
    where
        escaped = char '!' *> anyChar
        nonClose = noneOf ">"

groups :: Parser (Forest ())
groups = do
    char '{'
    gs <- group `sepBy` char ','
    char '}'
    return $ catMaybes gs

group :: Parser (Maybe (Tree ()))
group = Just <$> (Node () <$> groups) <|> (garbage *> pure Nothing)

rootGroup :: Parser (Tree ())
rootGroup = do
    Just g <- group
    return g

score :: Tree a -> Int
score t = sum $ zipWith f (levels t) [1..]
    where
        f :: [a] -> Int -> Int
        f as n = n * length as

main :: IO ()
main = do
    c <- readFile "input/09.input"
    let Right i = parse rootGroup "" c
    print $ score i
