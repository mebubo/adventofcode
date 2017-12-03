module Day03 where

lengths :: [Int]
lengths = [1..] >>= \x -> [x, x]

data Dir = E | N | W | S deriving (Show, Enum)

nextDir :: Dir -> Dir
nextDir S = E
nextDir x = succ x

type State = (Int, Dir)

states :: [State]
states = zip lengths (iterate nextDir E)

type Coord = (Int, Int)

inc :: State -> [Coord]
inc (n, E) = replicate n (1, 0)
inc (n, N) = replicate n (0, 1)
inc (n, W) = replicate n (-1, 0)
inc (n, S) = replicate n (0, -1)

incs :: [Coord]
incs = states >>= inc

coords :: [Coord]
coords = scanl f (0, 0) incs
    where
        f (x, y) (dx, dy) = (x + dx, y + dy)

manhattanDistance :: Coord -> Int
manhattanDistance (x, y) = abs x + abs y

distanceToNth :: Int -> Int
distanceToNth n = manhattanDistance $ coords !! (n - 1)

input1 :: Int
input1 = 265149

main :: IO ()
main = do
    print $ distanceToNth input1
