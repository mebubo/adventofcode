module Day11 where

import Math.Geometry.Grid.HexagonalInternal2 (HexDirection(..))
import Math.Geometry.Grid.Hexagonal2 (UnboundedHexGrid(..))
import Math.Geometry.Grid (neighbour, distance)
import Data.List.Split (wordsBy)
import Data.Maybe (fromJust)

type Input = [HexDirection]

readDirection :: String -> HexDirection
readDirection "n" = North
readDirection "nw" = Northwest
readDirection "ne" = Northeast
readDirection "s" = South
readDirection "sw" = Southwest
readDirection "se" = Southeast

readInput :: IO Input
readInput = (readDirection <$>) . wordsBy (==',') . filter (/='\n') <$> readFile "input/11.input"

type Index = (Int, Int)

step :: Index -> HexDirection -> Index
step ix = fromJust . neighbour UnboundedHexGrid ix

origin :: Index
origin = (0, 0)

steps :: Input -> Index
steps = foldl step origin

distanceFromOrigin :: Index -> Int
distanceFromOrigin = distance UnboundedHexGrid origin

path :: Input -> [Index]
path = scanl step origin

main :: IO ()
main = do
    i <- readInput
    print . distanceFromOrigin $ steps i
    print . maximum $ distanceFromOrigin <$> path i
