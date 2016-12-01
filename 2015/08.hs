import Data.Char

countCharacters :: [String] -> Int
countCharacters = sum . map length

diff :: [String] -> Int
diff ls = (countCharacters ls) - (countCharacters $ map readString ls)

readString :: String -> String
readString = replaceEscapeSequences . stripQuotes

stripQuotes :: String -> String
stripQuotes xs = tail $ init xs

replaceEscapeSequences :: String -> String
replaceEscapeSequences "" = ""
replaceEscapeSequences ('\\' : '\\' : xs) = '\\' : replaceEscapeSequences xs
replaceEscapeSequences ('\\' : '"' : xs) = ':' : replaceEscapeSequences xs
replaceEscapeSequences all@('\\' : 'x'  : a : b : xs) = if (isHexDigit a && isHexDigit b)
                                                        then '_' : replaceEscapeSequences xs
                                                        else (head all) : replaceEscapeSequences (tail all)
replaceEscapeSequences (x:xs) = x : replaceEscapeSequences xs

quote :: String -> String
quote [] = []
quote ('"' : xs) = "\\\"" ++ quote xs
quote ('\\' : xs) = "\\\\" ++ quote xs
quote (x : xs) = x : quote xs

addParens :: String -> String
addParens xs = "\"" ++ xs ++ "\""

encodeString :: String -> String
encodeString = addParens . quote

diff2 ls = (countCharacters $ map encodeString ls) - (countCharacters ls)

main = do
  input <- getContents
  print $ diff $ lines input
  print $ diff2 $ lines input
