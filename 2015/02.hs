wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

createBox :: String -> [Int]
createBox = map read . wordsWhen (=='x')

amountOfPaper :: [Int] -> Int
amountOfPaper xs =
  let 
      sides = zipWith (*) xs (drop 1 (cycle xs))
      minSide = minimum sides
  in
    minSide + (sum sides) * 2

lengthOfRibbon :: [Int] -> Int
lengthOfRibbon xs =
  let volume = product xs
      sidePerimeters = map (*2) (zipWith (+) xs (drop 1 (cycle xs)))
      smallestPerimeter = minimum sidePerimeters
  in
    volume + smallestPerimeter
      

solve :: [String] -> Int 
--solve =  sum . map amountOfPaper . map createBox 
solve = sum . map (lengthOfRibbon . createBox)

main = interact $ (++"\n") . show . solve . lines
