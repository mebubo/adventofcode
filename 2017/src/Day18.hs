-- Based on https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day18.hs

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day18 where

import Data.Char (isAlpha)
import qualified Data.List.PointedList as P
import Data.Maybe (fromJust)
import Data.Kind (Type)
import Control.Monad.Operational (Program, singleton, interpretWithMonad, ProgramT(..))
import Control.Monad.State (State, put, get, runState, execState)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)

type Addr = Either Char Int

data Op = OSnd Addr
        | ORcv Char
        | OJgz Addr Addr
        | OBin (Int -> Int -> Int) Char Addr

instance Show Op where
    show (OSnd x) = "OSnd " ++ show x
    show (ORcv x) = "ORcv " ++ show x
    show (OJgz x y) = "OJgz " ++ show x ++ " " ++ show y
    show (OBin _ x y) = "OBin " ++ show x ++ " " ++ show y

parseOp :: String -> Op
parseOp inp = case words inp of
        "snd":c:_ -> OSnd (addr c)
        "rcv":(c:_):_ -> ORcv c
        "jgz":x:y:_ -> OJgz (addr x) (addr y)
        "set":(x:_):y:_ -> OBin (const id) x (addr y)
        "add":(x:_):y:_ -> OBin (+) x (addr y)
        "mul":(x:_):y:_ -> OBin (*) x (addr y)
        "mod":(x:_):y:_ -> OBin mod x (addr y)
        _ -> error "Bad parse"
    where
        addr :: String -> Addr
        addr [c] | isAlpha c = Left c
        addr str = Right (read str)

parseProgram :: String -> P.PointedList Op
parseProgram = fromJust . P.fromList . (parseOp <$>) . lines

data StateCommand :: Type -> Type where
    Put :: Int -> StateCommand ()
    Get :: StateCommand Int

type IntState = Program StateCommand

putInt :: Int -> IntState ()
putInt = singleton . Put

getInt :: IntState Int
getInt = singleton Get

inc :: IntState ()
inc = do
    x <- getInt
    putInt (x + 1)

interpretIO :: IORef Int -> (StateCommand a -> IO a)
interpretIO r = \case
    Put x -> writeIORef r x
    Get -> readIORef r

runAsIO :: IntState a -> Int -> IO a
runAsIO m s0 = do
    r <- newIORef s0
    interpretWithMonad (interpretIO r) m

interpretState :: StateCommand a -> State Int a
interpretState = \case
    Put x -> put x
    Get -> get

runAsState :: IntState a -> State Int a
runAsState = interpretWithMonad interpretState

plus3 :: State Int ()
plus3 = runAsState plus3'

plus3' = do
    inc
    inc
    inc

-- instance Show (Program instr a) where
--     show (Lift x) = "lift"
--     show (Bind x) = "bind"
--     show (Instr x) = "instr"

main :: IO ()
main = do
    d <- readFile "input/18.input"
    let p = parseProgram d
    print p
    print $ execState plus3 1
    -- print plus3'
