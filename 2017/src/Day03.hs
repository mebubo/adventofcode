module Day03 where

lengths :: [Int]
lengths = [1..] >>= replicate 2

type Coord = (Int, Int)
type CoordTransform = (Int -> Int, Int -> Int)

transforms :: [CoordTransform]
transforms =
    [ ((+1), id)
    , (id, (+1))
    , ((+(-1)), id)
    , (id, (+(-1)))
    ]

allTransforms :: [CoordTransform]
allTransforms = concat $ zipWith replicate lengths (cycle transforms)

coords :: [Coord]
coords = scanl f (0, 0) allTransforms
    where
        f (x, y) (fx, fy) = (fx x, fy y)

manhattanDistance :: Coord -> Int
manhattanDistance (x, y) = abs x + abs y

distanceToNth :: Int -> Int
distanceToNth n = manhattanDistance $ coords !! (n - 1)

input1 :: Int
input1 = 265149

main :: IO ()
main = do
    print $ take 20 coords
    print $ distanceToNth input1
