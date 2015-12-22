module Day16 where

import Data.Map (fromList, Map, lookup)

type Tape = Map String (Int -> Bool)

tickerTape1 :: Tape
tickerTape1 = fromList [("children", (==3)),
                       ("cats", (==7)),
                       ("samoyeds", (==2)),
                       ("pomeranians", (==3)),
                       ("akitas", (==0)),
                       ("vizslas", (==0)),
                       ("goldfish", (==5)),
                       ("trees", (==3)),
                       ("cars", (==2)),
                       ("perfumes", (==1))
                      ]

tickerTape2 = fromList [("children", (==3)),
                       ("cats", (>7)),
                       ("samoyeds", (==2)),
                       ("pomeranians", (<3)),
                       ("akitas", (==0)),
                       ("vizslas", (==0)),
                       ("goldfish", (<5)),
                       ("trees", (>3)),
                       ("cars", (==2)),
                       ("perfumes", (==1))
                      ]

readAunt :: String -> [(String, Int)]
readAunt =
  toPairs . words . filtered
  where
    filtered = filter (\c -> c `notElem` ":,")
    toPairs :: [String] -> [(String, Int)]
    toPairs [] = []
    toPairs (x:y:ys) = (x, read y) : toPairs ys

isMatchingAunt :: Tape -> [(String, Int)] -> Bool
isMatchingAunt t = all f
                 where
                   f :: (String, Int) -> Bool
                   f (s, i) = case Data.Map.lookup s t of
                              Nothing -> True
                              Just f -> f i

showAunt :: [(String, Int)] -> String
showAunt = show . snd . head

main = do
  input <- getContents
  let solve tape = map showAunt . filter (isMatchingAunt tape) . map readAunt . lines
  print $ solve tickerTape1 input
  print $ solve tickerTape2 input
