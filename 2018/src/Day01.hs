module Day01 where

readNumber :: String -> Int
readNumber = read . (filter (/= '+'))

run :: IO ()
run = do
    ls <- lines <$> readFile "input/01.input"
    let is = map readNumber ls
    print $ sum is
