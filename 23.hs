module Day23 where

data Register = A | B deriving (Eq, Show)

data Instruction = Hlf Register | Tpl Register | Inc Register | Jmp Int | Jie Register Int | Jio Register Int deriving Show

data State = Running Int Int Int | Terminated deriving Show

type Program = [Instruction]

getInstruction :: Program -> State -> Instruction
getInstruction p (Running _ _ c) = p !! c

step :: Program -> State -> State
step p Terminated = Terminated
step p s@(Running a b c) | c >= length p || c < 0 = Terminated
                         | otherwise =
                             let i = getInstruction p s
                             in case i of
                             Hlf reg -> case reg of
                               A -> Running (a `div` 2) b (c+1)
                               B -> Running a (b `div` 2) (c+1)
                             Tpl reg -> case reg of
                               A -> Running (a * 3) b (c+1)
                               B -> Running a (b * 3) (c + 1)
                             Inc reg -> case reg of
                               A -> Running (a+1) b (c+1)
                               B -> Running a (b+1) (c+1)
                             Jmp offset -> Running a b (c + offset)
                             Jie reg offset -> Running a b (if (case reg of A -> a; B -> b) `mod` 2 == 0 then c + offset else c)
                             Jio reg offset -> Running a b (if (case reg of A -> a; B -> b) `mod` 2 == 1 then c + offset else c)
