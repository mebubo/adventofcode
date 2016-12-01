import Data.Set (size, fromList)
import Data.List (partition)

data Position = Position Int Int deriving (Eq, Ord)

offset :: Char -> (Int -> Int, Int -> Int)
offset x = case x of
  '>' -> ((+1), id)
  '<' -> ((subtract 1), id)
  '^' -> (id, (+1))
  'v' -> (id, (subtract 1))
  _ -> (id, id)

move :: Position -> (Int -> Int, Int -> Int) -> Position
move (Position x y) (fx, fy) = Position (fx x) (fy y)

startingPosition :: Position
startingPosition = Position 0 0

visited :: String -> [Position]
visited = foldl
          (\visited' x -> move (head visited') (offset x) : visited')
          [startingPosition]

uniq :: Ord a => [a] -> Int
uniq = size . fromList

bothVisited :: String -> [Position]
bothVisited xs = let 
  (santa', robot') = partition (odd . fst) (zip [0..] xs)
  (santa, robot) = (map snd santa', map snd robot')
  in visited santa ++ visited robot

main :: IO ()
-- main = interact $ (++"\n") . show . uniq . visited
main = interact $ (++"\n") . show . uniq . bothVisited
