{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Day24 where

import Data.List.Split (splitOn)
import Control.Monad.State.Lazy (StateT(StateT, runStateT), evalStateT)
import Control.Applicative (Alternative(empty, (<|>)))

type Component = (Int, Int)
type Input = [Component]

parseInput :: String -> Input
parseInput = (parseLine <$>) . lines
    where
        parseLine (splitOn "/" -> (x:y:_)) = (read x, read y)

readInput :: IO Input
readInput = parseInput <$> readFile "input/24.input"

select :: [a] -> [(a, [a])]
select [] = []
select (x:xs) = (x, xs) : (insert x <$> select xs)
    where
        insert x (a, ys) = (a, x:ys)

type State = StateT [Component] [] Int

-- taken from https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day24.hs
bridge :: Int -> State
bridge a = do
    (x, y) <- StateT select
    next <- if | a == x -> return y
               | a == y -> return x
               | otherwise -> empty
    rest <- return 0 <|> bridge next
    pure $ x + y + rest

main :: IO ()
main = do
    i <- readInput
    print i
    print $ select [1, 2, 3, 4]
    let s = runStateT (bridge 0) i
    print . maximum . (fst <$>) $ s
    let minLen = minimum $ ((length . snd) <$>) s
    print . maximum . (fst <$>) . filter ((==minLen) . length . snd) $ s
