module Day18 where

import Data.Array.Unboxed
import Data.List

type Grid = UArray (Int, Int) Bool

readGrid :: [String] -> Grid
readGrid lines' = array bounds' [ ((r, c), value v)
                                | (r, row) <- zip [0..] lines'
                                , (c, v) <- zip [0..] row]
  where bounds' = ((0, 0), (length lines' - 1, length (head lines') - 1))
        value '#' = True
        value '.' = False

validNeighbours :: Grid -> (Int, Int) -> [(Int, Int)]
validNeighbours g (r', c') = [(a, b)
                                    | a <- [r' - 1 .. r' + 1]
                                    , b <- [c' - 1 .. c' + 1]
                                    , (a, b) /= (r', c')
                                    , inRange (bounds g) (a, b)]
countNeighbours :: Grid -> (Int, Int) -> Int
countNeighbours g cs = length . filter (g!) $ validNeighbours g cs

step :: Grid -> Grid
step g = array bounds' [(i, (newValue g i)) | i <- range bounds']
  where bounds' = bounds g
        newValue :: Grid -> (Int, Int) -> Bool
        newValue g coord@(r, c) =
          let currentValue = g ! coord
              aliveNeighbours :: Int
              aliveNeighbours = countNeighbours g coord
              go :: Bool -> Int -> Bool
              go v n | v = n == 2 || n == 3
                     | otherwise = n == 3
          in
            go currentValue aliveNeighbours

step2 :: Grid -> Grid
step2 = turnCornersOn . step
  where
    turnCornersOn :: Grid -> Grid
    turnCornersOn g = g // [((r, c), True) | r <- [rLo, rHi], c <- [cLo, cHi]]
          where ((rLo,cLo),(rHi,cHi)) = bounds g

showGrid :: Grid -> String
showGrid g = intercalate "\n" [[ if g ! (r, c) then '#' else '.' | c <- [0..maxC]] | r <- [0..maxR]]
             where maxes = snd $ bounds g
                   maxR = fst maxes
                   maxC = snd maxes

countLights :: Grid -> Int
countLights g = length . filter id $ [g ! coord | coord <- range $ bounds g]

nSteps :: Int -> (Grid -> Grid) -> Grid -> Grid
nSteps n f g = iterate f g !! n

main = do
  input <- getContents
  let grid = readGrid $ lines input
      result = nSteps 100 step grid
      result2 = nSteps 100 step2 grid
  putStrLn $ showGrid result
  putStrLn ""
  putStrLn $ showGrid result2
  print $ countLights result
  print $ countLights result2
