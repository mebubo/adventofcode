module Day06 where

import Data.List (transpose, elemIndex, inits, nub, tails)
import Data.List.Split (chunksOf)

type Input = [Int]
type Result = Int

readInput :: IO Input
readInput = map read . words . head . lines <$> readFile "input/06.input"

realloc :: Int -> Int -> Int -> [Int]
realloc size start n = replicate start 0 ++ [-n] ++ replicate n 1 ++ replicate size 0

sumRealloc :: Int -> [Int] -> [Int]
sumRealloc n = map sum . transpose . chunksOf n

step :: [Int] -> [Int]
step xs = zipWith (+) xs reallocs
    where
        reallocs = sumRealloc siz $ realloc siz i v
        siz = length xs
        v = maximum xs
        Just i = elemIndex v xs

states :: Input -> [[Int]]
states = iterate step

heads :: Input -> [[[Int]]]
heads = inits . states

findRepeat :: Input -> [[Int]]
findRepeat = head . filter (\x -> last x `elem` init x) . tail . heads

solve1:: Input -> Result
solve1 = (+(-1)) . length . findRepeat

solve2:: Input -> Result
solve2 = (+(-1)) . length . head . filter (\x -> head x == last x) . tails . findRepeat

main :: IO ()
main = do
    i <- readInput
    print $ solve1 i
    print $ solve2 i
