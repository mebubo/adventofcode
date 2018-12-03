module Main where

import System.Environment (getArgs)

import qualified Day01

days :: [IO ()]
days =
    [ Day01.run ]

main :: IO ()
main = do
    args <- getArgs
    days !! ((read $ args !! 0) - 1)
