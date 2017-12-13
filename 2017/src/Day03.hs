module Day03 where

import Data.Monoid (Sum, getSum, (<>))

lengths :: [Int]
lengths = [1..] >>= replicate 2

type Coord = (Sum Int, Sum Int)

transforms :: [Coord]
transforms =
    [ (1, 0)
    , (0, 1)
    , (-1, 0)
    , (0, -1)
    ]

allTransforms :: [Coord]
allTransforms = concat $ zipWith replicate lengths (cycle transforms)

coords :: [Coord]
coords = scanl (<>) (0, 0) allTransforms

manhattanDistance :: Coord -> Int
manhattanDistance (x, y) = getSum $ abs x + abs y

distanceToNth :: Int -> Int
distanceToNth n = manhattanDistance $ coords !! (n - 1)

input1 :: Int
input1 = 265149

main :: IO ()
main = do
    print $ distanceToNth input1
