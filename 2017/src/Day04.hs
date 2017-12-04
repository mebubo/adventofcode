module Day04 where

import Data.List (group, sort)

type Input = [[String]]
type Result = Int

readInput :: IO Input
readInput = map words . lines <$> readFile "input/04.input"

solve1:: Input -> Result
solve1 = length . filter (all (==1) . map length . group . sort)

solve2 :: Input -> Result
solve2 = solve1 . map (map sort)

main :: IO ()
main = do
    i <- readInput
    print $ solve1 i
    print $ solve2 i
