module Main where

import Prelude hiding (Left, Right)
import Data.List.Split
import Data.List (tails)

data Coord = Coord Int Int deriving Eq

data Heading = North | East | South | West deriving (Enum, Show)

data State = State Coord Heading deriving Show

data Command = Command Turn Steps deriving Show

data Turn = Left | Right deriving Show

type Steps = Int

instance Show Coord
    where show (Coord x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

turn :: Turn -> Heading -> Heading
turn Left North = West
turn Left h = pred h
turn Right West = North
turn Right h = succ h

move :: State -> Steps -> Coord
move (State (Coord x y) h) s =
    case h of
        North -> Coord x (y+s)
        East  -> Coord (x+s) y
        South -> Coord x (y-s)
        West  -> Coord (x-s) y

step :: State -> Command -> State
step (State c h) (Command t s) =
    State newCoord newHeading
        where
            newHeading = turn t h
            newCoord = move (State c newHeading) s

readCommand :: String -> Command
readCommand ('L':ds) = Command Left (read ds)
readCommand ('R':ds) = Command Right (read ds)

readCommands :: String -> [Command]
readCommands = (map readCommand) . (splitOn ", ")

getCoord :: State -> Coord
getCoord (State c _) = c

distance :: Coord -> Coord -> Int
distance (Coord x1 y1) (Coord x2 y2) = (abs (x1-x2)) + (abs (y1-y2))

start :: Coord
start = Coord 0 0

initialState = State start North

path :: State -> [Command] -> [Coord]
path s = map getCoord . scanl step s

fill :: Coord -> Coord -> [Coord]
fill (Coord x1 y1) (Coord x2 y2) = [Coord x y | x <- range x1 x2, y <- range y1 y2]
    where
        range a b | a <= b = [a..b]
                  | otherwise = reverse [b..a]

expand :: [Coord] -> [Coord]
expand [x] = [x]
expand cs@(_:cs') = concat $ map tail $ zipWith fill cs cs'

firstVisitedTwice :: [Coord] -> Coord
firstVisitedTwice = head . head . dropWhile (not . headInTail) . tails
    where
        headInTail (h:t) = h `elem` t

main :: IO ()
main = do
    input <- getContents
    let commands = readCommands input
    let p = path initialState commands
    print . distance start . last $ p
    print . distance start . firstVisitedTwice . expand $ p
