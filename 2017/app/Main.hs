module Main where

import System.Environment (getArgs)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07

mains :: [IO ()]
mains =
    [ Day01.main
    , Day02.main
    , Day03.main
    , Day04.main
    , Day05.main
    , Day06.main
    , Day07.main
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> last mains
        (n:_) -> mains !! (read n - 1)
