import Data.Char

countCharacters :: [String] -> Int
countCharacters = sum . map length

diff :: [String] -> Int
diff ls = (countCharacters ls) - (countCharacters $ map readString ls)

readString :: String -> String
readString = replaceEscapeSequences . stripQuotes

stripQuotes :: String -> String
stripQuotes xs = drop 1 $ take (length xs - 1) xs

replaceEscapeSequences :: String -> String
replaceEscapeSequences "" = ""
replaceEscapeSequences ('\\' : '\\' : xs) = '\\' : replaceEscapeSequences xs
replaceEscapeSequences ('\\' : '"' : xs) = ':' : replaceEscapeSequences xs
replaceEscapeSequences all@('\\' : 'x'  : a : b : xs) = if (isHexDigit a && isHexDigit b)
                                                        then '_' : replaceEscapeSequences xs
                                                        else (head all) : replaceEscapeSequences (tail all)
replaceEscapeSequences (x:xs) = x : replaceEscapeSequences xs

main = do
  input <- getContents
  print $ diff $ lines input

