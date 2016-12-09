module Main where

type Screen = [[Bool]]

data Cmd = Rect Int Int
         | RotateRow Int Int
         | RotateCol Int Int

mapFirst :: Int -> (a -> a) -> [a] -> [a]
mapFirst n f as = (map f xs) ++ ys
    where (xs, ys) = splitAt n as

mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt n f as = xs ++ [f y] ++ zs
    where xs = take n as
          y = as !! n
          zs = drop (n+1) as

rect :: Int -> Int -> Screen -> Screen
rect cols rows = mapFirst rows (mapFirst cols (const True))

main :: IO ()
main = undefined
