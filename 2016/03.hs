module Main where

import Data.List.Split (splitOn, chunksOf)
import Data.List (transpose)

data Triangle = T Int Int Int

isValid :: Triangle -> Bool
isValid (T a b c) = a + b > c && b + c > a && c + a > b

readTriangle :: String -> Triangle
readTriangle = getTriangle . map read . words

getTriangle :: [Int] -> Triangle
getTriangle (a:b:c:_) = T a b c

main :: IO ()
main = do
    input <- getContents
    let triangles = map readTriangle . lines $ input
    print . length . filter isValid $ triangles
    let values = map words . lines $ input
    let triplets = chunksOf 3 . concat . transpose $ values
    let triangles2 = map (getTriangle . map read) $ triplets
    print . length . filter isValid $ triangles2
