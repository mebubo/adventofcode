module Day12 where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Megaparsec (Parsec, sepBy, parse, eof)
import Text.Megaparsec.Char (string, char, eol, digitChar)
import Control.Applicative (some, many)

type Input = M.Map Int (S.Set Int)

type Parser = Parsec () String

line :: Parser (Int, S.Set Int)
line = do
    k <- number
    string " <-> "
    vs <- number `sepBy` comma
    eol
    return (k, S.fromList vs)
        where
            comma = string ", "
            number = read <$> some digitChar

input :: Parser Input
input = M.fromList <$> many line <* eof

type State = (S.Set Int, S.Set Int)

add :: (Int, S.Set Int) -> Input -> State
add (x, curr) m = (xs, next)
    where
        new = M.findWithDefault S.empty x m
        next = curr `S.union` new
        xs = new `S.difference` curr

addAll :: State -> Input -> State
addAll (xs, curr) m | S.null xs = (xs, curr)
                    | otherwise = addAll (xs', curr') m
    where
        (ys, curr') = add (x, curr) m
        x = head $ S.toList xs
        xs' = ys `S.union` (x `S.delete` xs)

main :: IO ()
main = do
    i <- readFile "input/12.input"
    let Right m = parse input "" i
    print . length . snd $ addAll (S.singleton 0, S.empty) m
