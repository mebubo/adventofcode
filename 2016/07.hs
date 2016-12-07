module Main where

import Data.List
import Data.List.Split

isAbba :: String -> Bool
isAbba (a:b:c:d:_) = a == d && b == c && a /= b
isAbba _ = False

containsAbba :: String -> Bool
containsAbba = any isAbba . tails

data IP = IP [String] [String] deriving Show

supportsTLS :: IP -> Bool
supportsTLS (IP o i) = (any containsAbba o) && ((not . any containsAbba) i)

readIP :: String -> IP
readIP s = IP o i
    where xs = splitOneOf "[]" s
          ps = zip xs (cycle [0, 1])
          o = map fst . filter ((==0) . snd) $ ps
          i = map fst . filter ((==1) . snd) $ ps

main :: IO ()
main = do
    input <- fmap lines getContents
    print . length . filter (supportsTLS . readIP) $ input
