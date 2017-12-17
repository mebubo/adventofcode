module Day09 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Tree (Tree(..), Forest, levels, flatten)
import Data.Maybe (catMaybes, isNothing)
import Control.Monad (join)

type Parser = Parsec () String

type Garbage = Maybe String

garbage :: Parser Garbage
garbage = char '<' *> between <* char '>'
    where
        between = Just . catMaybes <$> many (escaped <|> nonClosed)
        escaped = Nothing <$ (char '!' *> anyChar)
        nonClosed = Just <$> noneOf ">"

groups :: Parser (Forest Garbage)
groups = char '{' *> group `sepBy` char ',' <* char '}'

group :: Parser (Tree Garbage)
group = grps <|> garb
    where
        grps = Node Nothing <$> groups
        garb = f <$> garbage
        f g = Node g []

score1 :: Tree Garbage -> Int
score1 t = sum $ zipWith f (levels t) [1..]
    where
        f :: [Garbage] -> Int -> Int
        f as n = n * length (filter isNothing as)

score2 :: Tree Garbage -> Int
score2 = length . join . catMaybes . flatten

main :: IO ()
main = do
    c <- readFile "input/09.input"
    let Right i = parse group "" c
    print $ score1 i
    print $ score2 i
