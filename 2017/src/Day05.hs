module Day05 where

import Data.Array.IO

type Input = IOUArray Int Int
type Result = Int

readInput :: IO Input
readInput = do
    xs <- map read . lines <$> readFile "input/05.input"
    newListArray (0, length xs) xs

type Inc = Int -> Int
type State = (Int, Int, Input)

solve :: Inc -> Input -> IO Int
solve f xs = loop (0, 0, xs)
    where
        loop :: State -> IO Int
        loop (n, i, xs) = do
            (a, b) <- getBounds xs
            if n >= b then
                return i
            else do
                x <- readArray xs n
                writeArray xs n (f x)
                loop (x + n, i + 1, xs)

solve1 = solve succ
solve2 = solve $ \x -> if x >= 3 then x - 1 else x + 1

main :: IO ()
main = do
    xs <- readInput
    print =<< solve1 xs
    xs <- readInput
    print =<< solve2 xs
