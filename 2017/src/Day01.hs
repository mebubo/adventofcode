module Day01 where

digits :: String -> [Int]
digits = map (read . pure)

allPairsOffsetBy :: Int -> [a] -> [(a, a)]
allPairsOffsetBy n xs = take (length xs) $ zipAdj $ xs ++ xs
    where zipAdj = zip <*> drop n

allNeighbours :: [a] -> [(a, a)]
allNeighbours = allPairsOffsetBy 1

allHalfwayNeighbours :: [a] -> [(a, a)]
allHalfwayNeighbours xs = allPairsOffsetBy (length xs `div` 2) xs

same :: Eq a => (a, a) -> Bool
same (a, b) = a == b

sumSame :: (Num a, Eq a) => [(a, a)] -> a
sumSame = sum . map fst . filter same

main :: IO ()
main = do
    d <- head . lines <$> readFile "input/01.input"
    print $ sumSame $ allNeighbours $ digits d
    print $ sumSame $ allHalfwayNeighbours $ digits d
