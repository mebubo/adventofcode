isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

factors :: Int -> [Int]
factors n = [x | x <- [1..(isqrt n)], n `mod` x == 0]

allFactors :: Int -> [Int]
allFactors n =
  let
    sf = factors n
    biggest = last sf
    nextToBiggest = n `div` biggest
    toSkip = if biggest == nextToBiggest then 1 else 0
    bf = drop toSkip [n `div` x | x <- reverse sf]
  in
    sf ++ bf

houseScore = (10*) . sum . allFactors

houseScore2 :: Int -> Int -> Int
houseScore2 max' n = (11*) . sum . filter ((<=max') . (n `div`)) $ allFactors n

matchingHouse :: (Int -> Int) -> Int -> Int
matchingHouse scoring i = fst . head . dropWhile ((<i) . snd) $ zip [1..] $ map scoring [1..]

main = do
  input <- getContents
  let i = read input
  print $ matchingHouse houseScore i
  print $ matchingHouse (houseScore2 50) i
