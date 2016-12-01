module Main where

import Prelude hiding (Left, Right)
import Data.List.Split

data Coord = Coord Int Int

data Heading = North | East | South | West deriving Enum

data State = State Coord Heading

data Command = Command Turn Steps

data Turn = Left | Right

type Steps = Int

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

travel :: State -> [Command] -> State
travel = foldl step

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

main :: IO ()
main = getContents >>=
    print . distance start . getCoord . travel initialState . readCommands
