module Main where

import Data.Hash.MD5

input :: String
input = "wtnhxymk"
-- input = "abc"

counter :: [Int]
counter = [0..]

hashInputs :: [String]
hashInputs = map (((++) input) . show) counter

hashes :: [String]
hashes = map (md5s . Str) hashInputs

matchingHashes :: [String]
matchingHashes = filter ((=="00000") . take 5) hashes

password :: String
password = map (head . drop 5) matchingHashes

combine :: String -> (Char, Char) -> String
combine s (p, c) | '0' <= p && p <='9' && p' < (length s) && s !! p' == '_' = set s p' c
                 | otherwise = s
    where p' = read [p]

set :: [a] -> Int -> a -> [a]
set l i e = take i l ++ e : drop (i+1) l

passwords2 :: [String]
passwords2 = scanl f "________" matchingHashes
    where
        f s (_:_:_:_:_:a:b:_) = combine s (a, b)

takeWhileAndNext :: (a -> Bool) -> [a] -> [a]
takeWhileAndNext f as = as' ++ [h]
    where
        (as', as'') = span f as
        (h:_) = as''

main :: IO ()
main = do
    let p = take 8 password
    mapM_ print $ scanl (\bs a -> bs ++ [a]) "" p
    mapM_ print . takeWhileAndNext (elem '_') $ passwords2
