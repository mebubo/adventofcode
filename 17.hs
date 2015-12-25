module Day17 where

combinations :: Int -> [Int] -> [[Int]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = combinations n xs ++ if n >= x then map (x:) (combinations (n-x) xs) else []

main = do
  input <- getContents
  print . length . combinations 150 . map read . lines $ input
