import Data.List (isInfixOf)

containsNOf :: Eq a=> Int -> [a] -> [a] -> Bool
containsNOf n ys xs = (>=n) $ sum $ map length $ map (\y -> filter (==y) xs) ys

contains3Vowels :: String -> Bool
contains3Vowels = containsNOf 3 "aeiou"

containsADouble :: Eq a => [a] -> Bool
containsADouble xs = any (uncurry (==)) $ zip xs (tail xs)

doesNotContainAnyOf :: Eq a => [[a]] -> [a] -> Bool
doesNotContainAnyOf yss xs = all id $ map (\ys -> not $ ys `isInfixOf` xs) yss

doesNotContainForbidenStrings :: String -> Bool
doesNotContainForbidenStrings = doesNotContainAnyOf ["ab", "cd", "pq", "xy"]

satisfiesAllConditions :: [String -> Bool] -> String -> Bool
satisfiesAllConditions fs ss = all (==True) $ map (\f -> f ss) fs

isNice :: String -> Bool
isNice = satisfiesAllConditions [contains3Vowels, containsADouble, doesNotContainForbidenStrings]

countNice :: [String] -> Int
countNice = length . filter (==True) . map isNice 
  
main = interact $ (++"\n") . show . countNice . lines
