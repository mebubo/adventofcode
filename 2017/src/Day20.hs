module Day20 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List (minimumBy, sortBy)
import Data.Ord (comparing)

type Parser = Parsec () String

int :: Parser Int
int = read <$> ((++) <$> sign <*> number)
    where
        sign = string "-" <|> pure ""
        number = some digitChar

type Coord = (Int, Int, Int)

coord :: Parser Coord
coord = char '<' *> tup <* char '>'
    where
        tup = (,,) <$> i <*> i <*> i
        i = int <* many (char ',')

params :: Parser (Coord, Coord, Coord)
params = tup <* eol
    where
        tup = (,,) <$> c <*> c <*> c
        c = anyChar *> char '=' *> coord <* many (string ", ")

absSq :: Coord -> Int
absSq (x, y, z) = x*x + y*y + z*z

input :: Parser [(Coord, Coord, Coord)]
input = many params <* eof

thrd :: (a, b, c) -> c
thrd (_, _, x) = x

main :: IO ()
main = do
    i <- readFile "input/20.input"
    let Right accs = parse input "" i
        accels = ((absSq . thrd) <$>) $ accs
        minAccel = minimum accels
    print . (fst <$>) . filter ((==minAccel) . snd) . zip [0..] $ accels
