intersperse :: a -> [a] -> [[a]]
intersperse x [] = [[x]]
intersperse x (y:ys) = (x:y:ys) : map (y:) (intersperse x ys)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concatMap (intersperse x) (permutations xs)

partitionInN :: Int -> [a] -> [[[a]]]
partitionInN _ [] = []
partitionInN 1 xs = [[xs]]
partitionInN n (x:xs) = map ([x]:) (partitionInN (n-1) xs) ++ concatMap (insert x) (partitionInN n xs)

insert :: a -> [[a]] -> [[[a]]]
insert x [] = []
insert x (ys:yss) = ((x:ys):yss) : map (ys:) (insert x yss)

isBalanced :: [[Int]] -> Bool
isBalanced xs = all (==(head sums)) $ sums
    where sums = map sum xs

firstGroupSize :: [[a]] -> Int
firstGroupSize = length . head

shortestFirstGroupsOnly :: [[[a]]] -> [[[a]]]
shortestFirstGroupsOnly xs =
    let minSize = minimum $ map firstGroupSize xs
     in filter ((==minSize) . firstGroupSize) xs

minimumEntanglement :: [[[Int]]] -> Int
minimumEntanglement = minimum . map (product . head)

main = do
    input <- getContents
    let weights = map read $ lines input
        a = (sum weights) `div` 3
        partitions = concatMap permutations . filter (all (==a) . (map sum)) $ partitionInN 3 weights
        balanced = filter isBalanced $ partitions
    print $ length weights
    print $ length partitions
    print $ minimumEntanglement $ shortestFirstGroupsOnly balanced
