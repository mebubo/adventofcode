module Day12 where

import Text.Regex.Posix

extractNumbers :: String -> [String]
extractNumbers xs = getAllTextMatches $ xs =~ "-?[[:digit:]]+" :: [String]

main = do
  input <- getContents
  let numbers = extractNumbers input
  print $ sum $ map read numbers
