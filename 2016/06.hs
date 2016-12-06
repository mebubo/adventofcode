module Main where

import Data.List

leastFrequent :: Ord a => [a] -> a
leastFrequent = head . head . sortOn length . group . sort

mostFrequent :: Ord a => [a] -> a
mostFrequent = head . head . sortOn (\xs -> - length xs) . group . sort

correct :: [String] -> String
correct = map mostFrequent . transpose

correct2 :: [String] -> String
correct2 = map leastFrequent . transpose

main :: IO ()
main = do
    input <- fmap lines getContents
    print $ correct input
    print $ correct2 input
