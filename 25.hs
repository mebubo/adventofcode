import Text.Regex.Posix

nextCoord :: (Int, Int) -> (Int, Int)
nextCoord (1, c) = (c+1, 1)
nextCoord (r, c) = (r-1, c+1)

cellOrder :: [(Int, Int)]
cellOrder = iterate nextCoord (1, 1)

nextValue :: Int -> Int
nextValue x = (x * 252533) `mod` 33554393

values :: [Int]
values = iterate nextValue 20151125

valueForCoord :: (Int, Int) -> Int
valueForCoord coord = values !! count
  where count = length $ takeWhile (/= coord) cellOrder

valueForCoord2 :: (Int, Int) -> Int
valueForCoord2 coord = snd . head . filter ((==coord) . fst) $ zip cellOrder values

valueForCoord3 :: (Int, Int) -> Int
valueForCoord3 (r, c) = iter $ zip cellOrder values
  where
    iter (((r', c'), z):rest) | r == r' && c == c' = z
                              | otherwise = r' `seq` c' `seq` z `seq` (iter rest)

main = do
  input <- getContents
  let [r, c] = map (read::(String -> Int)) . getAllTextMatches $ input =~ "[[:digit:]]+"
  print $ valueForCoord (r, c)
