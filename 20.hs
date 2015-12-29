module Day20 where

import Data.List

elfs :: [[Int]]
elfs = [[if h `mod` e == 0 then 10*e else 0 | h <- [1..]] | e <- [1..]]

houses = transpose elfs

housePoints = map (sum . uncurry take) $  zip [1..] houses

input = 29000

matchingHouse = fst . head . dropWhile ((<input) . snd) $ zip [1..] housePoints

main = print matchingHouse
