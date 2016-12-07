module Main where

import Data.List
import Data.List.Split
import Data.Tuple

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

isAba :: String -> Bool
isAba (a:b:c:_) = a == c && a /= b
isAba _ = False

type Aba = (Char, Char)

getAbas :: String -> [Aba]
getAbas = map c . filter isAba . tails
    where
        c (a:b:_) = (a, b)

getBabs :: String -> [Aba]
getBabs = map swap . getAbas

supportsSSL :: IP -> Bool
supportsSSL (IP o i) = (>0) . length $ intersect abas babs
    where abas = concatMap getAbas o
          babs = concatMap getBabs i

main :: IO ()
main = do
    input <- fmap lines getContents
    let ips = map readIP input
    print . length . filter supportsTLS $ ips
    print . length . filter supportsSSL $ ips
