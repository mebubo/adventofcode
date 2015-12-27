module Day23 where

data Register = A | B deriving (Eq, Show)

data Instruction = Hlf Register | Tpl Register | Inc Register | Jmp Int | Jie Register Int | Jio Register Int deriving Show

data State = Running Int Int Int | Terminated deriving (Eq, Show)

type Program = [Instruction]

getInstruction :: Program -> State -> Instruction
getInstruction p (Running _ _ c) = p !! c

step :: Program -> State -> State
step _ Terminated = Terminated
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
                             Jie reg offset -> Running a b (if (case reg of A -> a; B -> b) `mod` 2 == 0 then c + offset else c+1)
                             Jio reg offset -> Running a b (if (case reg of A -> a; B -> b) == 1 then c + offset else c+1)

execution :: Program -> State -> [State]
execution p s = takeWhile (/=Terminated) $ iterate (step p) s

finalValueInRegB :: [State] -> Int
finalValueInRegB = getB . last
  where getB (Running _ b _) = b

readInstruction :: String -> Instruction
readInstruction s =
  let ws = words $ filter (`notElem` ",+") s
      strToReg "a" = A
      strToReg "b" = B
  in case ws of
  ["hlf", reg] -> Hlf $ strToReg reg
  ["tpl", reg] -> Tpl $ strToReg reg
  ["inc", reg] -> Inc $ strToReg reg
  ["jmp", offset] -> Jmp $ read offset
  ["jie", reg, offset] -> Jie (strToReg reg) (read offset)
  ["jio", reg, offset] -> Jio (strToReg reg) (read offset)

main = do
  input <- getContents
  let program = map readInstruction . lines $ input
      exec = execution program $ Running 0 0 0
      exec2 = execution program $ Running 1 0 0
  print $ finalValueInRegB exec
  print $ finalValueInRegB exec2
