import Data.Map (Map, fromList, keys, (!))
import Data.List (nub)

data Conn = Conn String String deriving (Show, Eq, Ord)

conn :: String -> String -> Conn
conn x y
  | x <= y = Conn x y
  | otherwise = Conn y x

readInput :: [String] -> Map Conn Int
readInput = fromList . map (r . words)
  where
    r :: [String] -> (Conn, Int)
    r [x, "to", y, "=", z] = (conn x y, read z)

cities :: Map Conn Int -> [String]
cities = nub . concatMap endpoints . keys
  where 
    endpoints (Conn x y) = [x, y]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concatMap (intersperse x) (permutations xs)

intersperse :: a -> [a] -> [[a]]
intersperse x [] = [[x]]
intersperse x (y:ys) = (x:y:ys) : map (y:) (intersperse x ys)

routeLength :: Map Conn Int -> [String] -> Int
routeLength m cities = sum [m ! conn | conn <- connections cities]
  where
    connections :: [String] -> [Conn]
    connections xs = zipWith conn xs (tail xs)

allRouteLengths :: Map Conn Int -> [Int]
allRouteLengths m = map (routeLength m) (permutations $ cities m)

main = do
  input <- getContents
  let m = readInput $ lines input
      lengths = allRouteLengths m
  print $ minimum $ lengths
  print $ maximum $ lengths
