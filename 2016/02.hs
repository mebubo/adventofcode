data Instruction = U | R | D | L deriving Read

data Coord = Coord Int Int

type Key = Int

move :: Instruction -> Coord -> Coord
move U (Coord r c) = Coord (r-1) c
move R (Coord r c) = Coord r (c+1)
move D (Coord r c) = Coord (r+1) c
move L (Coord r c) = Coord r (c-1)

contain :: Coord -> Coord
contain (Coord x y) = Coord (contain' x) (contain' y)
    where contain' a | a < 0 = 0
                    | a > 2 = 2
                    | otherwise = a

containedMove :: Coord -> Instruction -> Coord
containedMove c i = contain $ move i c

getKey :: Coord -> Key
getKey (Coord r c) = 3*r + c + 1

decodeKey :: Coord -> [Instruction] -> Key
decodeKey c = getKey . foldl containedMove c

readInstructions :: String -> [Instruction]
readInstructions = map (read . (:[]))

start :: Coord
start = Coord 1 1

main :: IO ()
main = do
    input <- getContents
    let inst = map readInstructions . lines $ input
    print $ concatMap (show . decodeKey start) inst
