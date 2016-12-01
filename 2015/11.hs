module Day11 where

import Data.Char

zero = ord 'a'

base = (ord 'z' - zero) + 1

toDigit :: Char -> Int
toDigit x = ord x - zero

toChar :: Int -> Char
toChar x = chr (x + zero)

incrementDigit :: Char -> Int -> (Int, Char)
incrementDigit x y =
  let s = toDigit x + y
  in (s `div` base, toChar (s `mod` base))

incrementNumber :: [Char] -> Int -> (Int, [Char])
incrementNumber [] y = (y, [])
incrementNumber (x:xs) y =
  let
    (carry, xs') = incrementNumber xs y
    (carry', x') = incrementDigit x carry
  in
    (carry', x':xs')

increment :: String -> String
increment [] = []
increment xs =
  let (carry, xs') = incrementNumber xs 1
  in case carry of
          0 -> xs'
          _ -> toChar carry : xs'


containsAnyOf :: String -> String -> Bool
containsAnyOf ys xs = or [y `elem` xs | y <- ys]

containsNPairs :: Int -> String -> Bool
containsNPairs 0 _ = True
containsNPairs _ [] = False
containsNPairs _ [x] = False
containsNPairs n (x:y:ys) = if x == y
                            then containsNPairs (n-1) ys
                            else containsNPairs n (y:ys)


containsIncreasingSubsequenceOfSize :: Int -> String -> Bool
containsIncreasingSubsequenceOfSize n xs =
  let
    ints = map ord xs
    diffs = zipWith (-) (tail ints) ints

    startsWith [] xs = True
    startsWith _ [] = False
    startsWith (y:ys) (x:xs) = (y == x) && startsWith ys xs

    contains ys [] = False
    contains ys (x:xs) = startsWith ys (x:xs) || contains ys xs
  in
    contains (replicate (n-1) 1) diffs


isGoodPassword :: String -> Bool
isGoodPassword =
  satisfiesAllConditions [containsIncreasingSubsequenceOfSize 3, not . containsAnyOf "iol", containsNPairs 2]
  where
    satisfiesAllConditions conds xs = all (\f -> f xs) conds

input = "hxbxwxba"

passwords = iterate increment input

goodPasswords = filter isGoodPassword passwords

main = print $ take 2 goodPasswords
