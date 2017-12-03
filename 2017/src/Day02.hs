module Day02 where

type Input = [[Int]]
type Output = Int

readInput :: IO Input
readInput = map (map read . words) . lines <$> readFile "input/02.input"

minMaxDiff :: (Ord a, Num a) => [a] -> a
minMaxDiff = (-) <$> maximum <*> minimum

solve1 :: Input -> Output
solve1 = sum . map minMaxDiff

allPairs :: [a] -> [(a, a)]
allPairs xs = (,) <$> xs <*> xs

divisible :: Integral a => (a, a) -> Bool
divisible (a, b) = a /= b && a `mod` b == 0

divide :: [Int] -> Int
divide = uncurry div . head . filter divisible . allPairs

solve2 :: Input -> Output
solve2 = sum . map divide

main :: IO ()
main = do
    d <- readInput
    print $ solve1 d
    print $ solve2 d
