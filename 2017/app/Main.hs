module Main where

import System.Environment (getArgs)

import qualified Day01
import qualified Day02

mains :: [IO ()]
mains =
    [ Day01.main
    , Day02.main
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> last mains
        (n:_) -> mains !! (read n - 1)
