import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map

containsNOf :: Eq a=> Int -> [a] -> [a] -> Bool
containsNOf n ys xs = (>=n) $ length $ filter (`elem` xs) ys

doesNotContainAnyOf :: Eq a => [[a]] -> [a] -> Bool
doesNotContainAnyOf yss xs = all id $ map (\ys -> not $ ys `isInfixOf` xs) yss

containsADoubleSeparatedBy :: Eq a => Int -> [a] -> Bool
containsADoubleSeparatedBy n xs = any (uncurry (==)) $ zip xs (drop (n+1) xs)

containsNonOverlappingSublistsOfLength :: Ord a => Int -> [a] -> Bool
containsNonOverlappingSublistsOfLength n =
  (>0) . length . filter (>=n) . fartherstOccurences . multipleOccurences . listsOfIndexes . toMapOfListsOfIndexes . subListsOfLength n
  where
    subListsOfLength :: Int -> [a] -> [[a]]
    subListsOfLength n xs = takeWhile ((==n) . length) $ map (take n) $ map (\m -> drop m xs) [0..]

    toMapOfListsOfIndexes :: Ord a => [[a]] -> Map.Map [a] [Int]
    toMapOfListsOfIndexes ss = Map.fromListWith (++) $ zip ss (map (\x -> [x]) [0..])

    listsOfIndexes :: Map.Map [a] [Int] -> [[Int]]
    listsOfIndexes = Map.elems

    multipleOccurences :: [[a]] -> [[a]]
    multipleOccurences = filter ((>1) . length) 

    fartherstOccurences :: [[Int]] -> [Int]
    fartherstOccurences = map (\xs -> abs $ (last xs) - (head xs))

satisfiesAllConditions :: [String -> Bool] -> String -> Bool
satisfiesAllConditions fs ss = all (\f -> f ss) fs

countNice :: (String -> Bool) -> [String] -> Int
countNice f = length . filter f
  
isNice :: String -> Bool
isNice = satisfiesAllConditions
         [
           containsNOf 3 "aeiou",
           containsADoubleSeparatedBy 0,
           doesNotContainAnyOf ["ab", "cd", "pq", "xy"]
         ]

isNice2 :: String -> Bool
isNice2 = satisfiesAllConditions
         [
           containsADoubleSeparatedBy 1,
           containsNonOverlappingSublistsOfLength 2
         ]

-- main = interact $ (++"\n") . show . countNice isNice . lines
main = interact $ (++"\n") . show . countNice isNice2 . lines
