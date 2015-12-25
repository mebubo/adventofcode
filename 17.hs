module Day17 where

combinations :: Int -> [Int] -> [[Int]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = combinations n xs ++ if n >= x then map (x:) (combinations (n-x) xs) else []

minimumLength :: [[a]] -> [[a]]
minimumLength xs = filter (\ys -> length ys == m) xs
  where m = minimum . map length $ xs

main = do
  input <- getContents
  let containers = map read . lines $ input
      matches = combinations 150 containers
  print . length $ matches
  print . length . minimumLength $ matches
