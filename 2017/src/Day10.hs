module Day10 where

import Data.List.Split (wordsBy)
import Data.Vector (toList, fromList, (//))

type Input = [Int]

readInput :: IO Input
readInput = (read <$>) . wordsBy (==',') <$> readFile "input/10.input"

type State = ([Int], Int, Int)

step :: Int -> State -> State
step n (xs, pos, skip) = (xs', pos', skip')
    where
        skip' = skip + 1
        pos' = (pos + n + skip) `mod` length xs
        xs' = update xs pos n

update :: [a] -> Int -> Int -> [a]
update as pos siz = toList . f . fromList $ as
    where
        f vs = vs // zip ixs xs
        len = length as
        ixs = (`mod` len) <$> [pos..pos + siz - 1]
        xs = reverse $ (as !!) <$> ixs

steps :: [Int] -> State -> State
steps ns = foldl (flip (.)) id $ step <$> ns

initial :: State
initial = ([0..255], 0, 0)

main :: IO ()
main = do
    i <- readInput
    print $ product $ take 2 $ (\(a, _, _) -> a) $ steps i initial
