module Main where

import Text.Regex.Posix
import Data.List

data Room = Room { name :: String
                 , sectorId :: Int
                 , checksum :: String
                 } deriving Show

roomRegex :: String
roomRegex = "([[:alpha:]-]*)([[:digit:]]+)[[](.*)[]]"

readRoom :: String -> Room
readRoom str = Room (dropDashes n) (read s) c
    where
        result = str =~ roomRegex :: [[String]]
        (n:s:c:_) = tail . head $ result
        dropDashes = filter (/='-')

nMostCommon :: Ord a => Int -> [a] -> [a]
nMostCommon n = take n . map head . sortOn key . group . sort
    where key s = (- length s, s)

isValidRoom :: Room -> Bool
isValidRoom (Room n _ c) = c == nMostCommon 5 n

sumSectorIds :: [Room] -> Int
sumSectorIds = sum . map sectorId

rotate :: Int -> Char -> Char
rotate n c = toEnum (a + (c' - a + n) `mod` sz)
    where a = fromEnum 'a'
          z = fromEnum 'z'
          c' = fromEnum c
          sz = z - a + 1

rotateString :: Int -> String -> String
rotateString n = map (rotate n)

decodeName :: Room -> String
decodeName r = rotateString (sectorId r) (name r)

findNorthPole :: [Room] -> Room
findNorthPole = head . dropWhile ((/="northpoleobjectstorage") . decodeName)

main :: IO ()
main = do
    input <- fmap lines getContents
    let rooms = map readRoom $ input
    print . sumSectorIds . filter isValidRoom $ rooms
    -- _ <- sequence . fmap print . map decodeName $ rooms
    -- mapM_ print . map decodeName $ rooms
    print . sectorId . findNorthPole $ rooms
