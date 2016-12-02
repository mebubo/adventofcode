import Numeric (showHex)

data Instruction = U | R | D | L deriving Read

data Coord = Coord Int Int deriving Show

type Key = String

type Move = Coord -> Instruction -> Coord

move :: Move
move (Coord r c) U = Coord (r-1) c
move (Coord r c) R = Coord r (c+1)
move (Coord r c) D = Coord (r+1) c
move (Coord r c) L = Coord r (c-1)

getKey1 :: Coord -> Key
getKey1 (Coord r c) = show $ 3*r + c + 1

getKey2 :: Coord -> Key
getKey2 (Coord r c) = showHex (5*r + r' + c - 1)  ""
    where r' = sum . take r $ [-3, -1, -1, -3]

findCoord :: Move -> Coord -> [Instruction] -> Coord
findCoord m c = foldl m c

readInstructions :: String -> [Instruction]
readInstructions = map (read . (:[]))

start1 :: Coord
start1 = Coord 1 1

start2 :: Coord
start2 = Coord 2 0

within1 :: Coord -> Bool
within1 (Coord r c) = 0 <= r && r <=2 && 0 <= c && c <= 2

within2 :: Coord -> Bool
within2 (Coord r c) | r == 0 || r == 4 = c == 2
                    | r == 1 || r == 3 = 1 <= c && c <= 3
                    | r == 2 = 0 <= c && c <= 4
                    | otherwise = False

containedMove :: (Coord -> Bool) -> Coord -> Instruction -> Coord
containedMove within c i =
    let candidate = move c i
    in
        if within candidate then candidate else c

main :: IO ()
main = do
    input <- getContents
    let inst = map readInstructions . lines $ input
    print $ concatMap (getKey1 . findCoord (containedMove within1) start1) inst
    print $ concatMap (getKey2 . findCoord (containedMove within2) start2) inst
