import Data.List (tails)

combinationsOfLength :: Int -> [a] -> [[a]]
combinationsOfLength 0 _ = [[]]
combinationsOfLength _ [] = []
combinationsOfLength n xs = [ y:zs | (y:ys) <- tails xs, zs <- combinationsOfLength (n-1) ys ]

allCombinations :: [a] -> [[a]]
allCombinations xs = concatMap (\n -> combinationsOfLength n xs) [1..]

combinationsSummingUpTo :: Int -> [[Int]] -> [[Int]]
combinationsSummingUpTo n = filter ((==n) . sum)

shortestCombinations :: [[a]] -> [[a]]
shortestCombinations (x:xs) = takeWhile ((==(length x)) . length) (x:xs)

minimumEntanglement :: [[Int]] -> Int
minimumEntanglement = minimum . map product

solve :: Int -> [Int] -> Int
solve n = minimumEntanglement . shortestCombinations . combinationsSummingUpTo n . allCombinations

main = do
    input <- getContents
    let weights = map read $ lines input
        w1 = (sum weights) `div` 3
        w2 = (sum weights) `div` 4
    print $ solve w1 weights
    print $ solve w2 weights
