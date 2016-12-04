module Main where

import Data.List.Split (splitOn, chunksOf)
import Data.List (transpose)

data Triangle = T Int Int Int

isValid :: Triangle -> Bool
isValid (T a b c) = a + b > c && b + c > a && c + a > b

getTriangle :: [Int] -> Triangle
getTriangle (a:b:c:_) = T a b c

getTriangles :: [Int] -> [Triangle]
getTriangles = map getTriangle . chunksOf 3

values1 :: [String] -> [Int]
values1 = map read . words . concat

values2 :: [String] -> [Int]
values2 = concat . transpose . map ((map read) . words)

countValidTriangles :: [Triangle] -> Int
countValidTriangles = length . filter isValid

main :: IO ()
main = do
    input <- fmap lines getContents
    p . values1 $ input
    p . values2 $ input
    where p = print . countValidTriangles . getTriangles
