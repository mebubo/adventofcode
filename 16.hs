module Day16 where

import Data.Map (fromList, Map, lookup)

tickerTape :: Map String Int
tickerTape = fromList [("children", 3),
                       ("cats", 7),
                       ("samoyeds", 2),
                       ("pomeranians", 3),
                       ("akitas", 0),
                       ("vizslas", 0),
                       ("goldfish", 5),
                       ("trees", 3),
                       ("cars", 2),
                       ("perfumes", 1)
                      ]

readAunt :: String -> [(String, Int)]
readAunt =
  toPairs . words . filtered
  where
    filtered = filter (\c -> c `notElem` ":,")
    toPairs :: [String] -> [(String, Int)]
    toPairs [] = []
    toPairs (x:y:ys) = (x, read y) : toPairs ys

isMatchingAunt :: [(String, Int)] -> Bool
isMatchingAunt = all f
                 where
                   f :: (String, Int) -> Bool
                   f (s, i) = case Data.Map.lookup s tickerTape of
                              Nothing -> True
                              Just i' -> i == i'

showAunt :: [(String, Int)] -> String
showAunt = show . snd . head

main = do
  input <- getContents
  print $ (map showAunt) . (filter isMatchingAunt) . (map readAunt) . lines $ input
