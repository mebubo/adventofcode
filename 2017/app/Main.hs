module Main where

import System.Environment (getArgs)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day18
import qualified Day20
import qualified Day24

mains :: [IO ()]
mains =
    [ Day01.main
    , Day02.main
    , Day03.main
    , Day04.main
    , Day05.main
    , Day06.main
    , Day07.main
    , Day08.main
    , Day09.main
    , Day10.main
    , Day11.main
    , Day12.main
    , Day18.main
    , Day20.main
    , Day24.main
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> last mains
        (n:_) -> mains !! (read n - 1)
