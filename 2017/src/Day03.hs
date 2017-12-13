module Day03 where

import Data.Monoid (Sum(Sum), getSum, (<>))
import qualified Data.Map as M
import Control.Monad.State.Lazy (State, get, put, evalState, runState)
import Data.Maybe (mapMaybe)

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

type SpiralMap = M.Map Coord Int
type SpiralState = State SpiralMap Int

stateForCoord :: Coord -> SpiralState
stateForCoord coord = do
    m <- get
    let n = sumAdjacent coord m
    put $ M.insert coord n m
    return n

sumAdjacent :: Coord -> SpiralMap -> Int
sumAdjacent (0, 0) _ = 1
sumAdjacent coord m = sum . mapMaybe (`M.lookup` m) $ adjs
    where
        adjs :: [Coord]
        adjs = [coord <> (a, b) | a <- range, b <- range, a /= 0 || b /= 0]
        range = Sum <$> [-1..1]

values :: [Coord] -> State SpiralMap [Int]
values = traverse stateForCoord

input :: Int
input = 265149

main :: IO ()
main = do
    print $ distanceToNth input
    print $ head . filter (> input) $ evalState (values coords) M.empty
