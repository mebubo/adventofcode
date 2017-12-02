module Day02 where

minMax :: Ord a => [a] -> (a, a)
minMax = (,) <$> minimum <*> maximum

minMaxDiff :: (Ord a, Num a) => [a] -> a
minMaxDiff xs = b - a
    where (a, b) = minMax xs

main :: IO ()
main = do
    d <- map (map read . words) . lines <$> readFile "input/02.input"
    let diffs = map minMaxDiff d
    print $ sum diffs
