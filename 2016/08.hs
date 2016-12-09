module Main where

import Data.List (transpose, isInfixOf)
import Data.List.Split (splitOn)
import Data.Char (isDigit)

type Screen = [[Bool]]

data Cmd = Rect Int Int
         | RotateRow Int Int
         | RotateCol Int Int
         deriving Show

mapFirst :: Int -> (a -> a) -> [a] -> [a]
mapFirst n f as = (map f xs) ++ ys
    where (xs, ys) = splitAt n as

mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt n f as = xs ++ [f y] ++ zs
    where xs = take n as
          y = as !! n
          zs = drop (n+1) as

rect :: a -> Int -> Int -> [[a]] -> [[a]]
rect a cols rows = mapFirst rows (mapFirst cols (const a))

rotateRow :: Int -> Int -> [[a]] -> [[a]]
rotateRow row amount = mapAt row (rotate amount)

rotateColumn :: Int -> Int -> [[a]] -> [[a]]
rotateColumn col amount = transpose . rotateRow col amount . transpose

rotate :: Int -> [a] -> [a]
rotate n as = take l . drop (l - n) . cycle $ as
    where l = length as

readCmd :: String -> Cmd
readCmd s | "row" `isInfixOf` s = RotateRow a b
          | "column" `isInfixOf` s = RotateCol a b
          | otherwise = Rect a b
          where
            clearIfNonDigit c = if isDigit c then c else ' '
            (a:b:_) = map read . filter ((>0) . length) . splitOn " " $ map clearIfNonDigit s

step :: Screen -> Cmd -> Screen
step s c = case c of
    Rect cols rows -> rect True cols rows s
    RotateRow row amount -> rotateRow row amount s
    RotateCol col amount -> rotateColumn col amount s

screen :: Screen
screen = replicate 6 (replicate 50 False)

main :: IO ()
main = do
    cmds <- fmap (map readCmd . lines) getContents
    let result = foldl step screen cmds
    print . length . filter id . concat $ result
    let translate True = '#'
        translate False = ' '
    mapM_ (print . map translate) result
