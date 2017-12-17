module Day10 where

import Data.List.Split (wordsBy, chunksOf)
import Data.Vector (toList, fromList, (//))
import Data.Char (ord)
import Data.Bits (xor)
import Control.Monad (join)
import Text.Printf (printf)

type RawInput = String
type Input = [Int]

readInput :: IO RawInput
readInput = readFile "input/10.input"

getInput :: RawInput -> Input
getInput = (read <$>) . wordsBy (==',')

getInput2 :: RawInput -> Input
getInput2 = (ord <$>) . filter (/= '\n')

type State = ([Int], Int, Int)

step :: Int -> State -> State
step n (xs, pos, skip) = (xs', pos', skip')
    where
        skip' = skip + 1
        pos' = (pos + n + skip) `mod` length xs
        xs' = update xs pos n

update :: [a] -> Int -> Int -> [a]
update as pos siz = toList . f . fromList $ as
    where
        f vs = vs // zip ixs xs
        len = length as
        ixs = (`mod` len) <$> [pos..pos + siz - 1]
        xs = reverse $ (as !!) <$> ixs

steps :: [Int] -> State -> State
steps ns = foldl (flip (.)) id $ step <$> ns

initial :: State
initial = ([0..255], 0, 0)

fst' :: State -> [Int]
fst' (a, _, _) = a

suffix = [17, 31, 73, 47, 23]

steps2 :: [Int] -> State -> State
steps2 = steps . concat . replicate 64 . (++ suffix)


postProcess :: [Int] -> String
postProcess xs = toString =<< xorAll <$> chunksOf 16 xs
    where
        xorAll :: [Int] -> Int
        xorAll = foldl xor 0
        toString :: Int -> String
        toString = printf "%02x"

main :: IO ()
main = do
    i <- readInput
    print . product . take 2 . fst' . flip steps initial . getInput $ i
    print . postProcess . fst' . flip steps2 initial . getInput2 $ i
