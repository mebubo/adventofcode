module Day14 where

import Text.Regex.Posix

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
solution = maximum . map (distance 2503 0 True)

main = do
  input <- getContents
  print $ solution $ map parseReindeer $ lines input
