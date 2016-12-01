module Day14 where

import Text.Regex.Posix
import Data.List (transpose, findIndices)

raceDuration :: Int
raceDuration = 2503

type Reindeer = (Int, Int, Int)

parseReindeer :: String -> Reindeer
parseReindeer xs = tuple $ map read $ getAllTextMatches $ xs =~ "[[:digit:]]+"
  where
    tuple [a, b, c] = (a, b, c)
    tuple x = error $ "Wrong format: " ++ show x


distance :: Int -> Int -> Bool -> Reindeer -> Int
distance time traveled isRacing deer@(speed, timeRacing, timeResting) 
  | not isRacing && time <= timeResting = traveled
  | isRacing && time <= timeRacing = traveled + speed * time
  | not isRacing = distance (time - timeResting) traveled (not isRacing) deer
  | otherwise = distance (time - timeRacing) (traveled + d) (not isRacing) deer
  where d = speed * timeRacing

solution :: [Reindeer] -> Int
solution = maximum . map (distance raceDuration 0 True)

movements :: Reindeer -> [Int]
movements (speed, timeRacing, timeResting) = concat . repeat $ replicate timeRacing speed ++ replicate timeResting 0

positions :: Reindeer -> [Int]
positions = scanl1 (+) . movements

pointsForPosition :: [Int] -> [Int]
pointsForPosition xs = map (oneOrZero $ maximum xs) xs
  where oneOrZero a b = if a == b then 1 else 0

positionsToPoints :: [[Int]] -> [[Int]]
positionsToPoints = transpose . map pointsForPosition . transpose

solution2 :: [Reindeer] -> Int
solution2 = maximum . map sum . positionsToPoints . map (take raceDuration . positions)

main = do
  input <- getContents
  print . solution . map parseReindeer . lines $ input
  print . solution2 . map parseReindeer . lines $ input
