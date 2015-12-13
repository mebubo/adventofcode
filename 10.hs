repeats :: Eq a => [a] -> [[a]]
repeats [] = [[]]
repeats (x:xs) =
  let rest = repeats xs
  in case head rest of
    [] -> [[x]]
    all@(y:ys) -> if y == x
                  then (x:all) : (tail rest)
                  else [x] : rest

say :: [Int] -> [Int]
say xs = [length xs, head xs]

input = "1113222113"

step :: [Int] -> [Int]
step = concat . map say . repeats

toListOfInt :: String -> [Int]
toListOfInt = map (read . (:[])) 

toString :: [Int] -> String
toString = concat . map show
              
nSteps :: Int -> a -> (a -> a) -> a
nSteps n start f = iterate f start !! n

solution :: Int -> Int
solution n = length . toString $ nSteps n (toListOfInt input) step
                   
main = do
  print $ solution 40
  print $ solution 50
