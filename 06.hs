data Action = SwitchOff | SwitchOn | Toggle deriving Show

data Point = Point Int Int deriving Show

data Square = Square Point Point deriving Show

within :: Point -> Square -> Bool
within (Point x y) (Square (Point a b) (Point c d)) = a <= x && x <= c && b <= y && y <= d

data State = On | Off deriving (Show, Eq)

data Transform = Transform Action Square deriving Show

performAction :: Action -> State -> State
performAction SwitchOff _ = Off
performAction SwitchOn _ = On
performAction Toggle On = Off
performAction Toggle Off = On

createTransform :: String -> Transform
createTransform s =
  let ws = words s
      isToggle = (head ws) == "toggle"
      isTurnOn = (take 2 ws) == ["turn", "on"]
      isTurnOff = (take 2 ws) == ["turn", "off"]
      rest = drop (if isToggle then 1 else 2) ws
      topLeft = head rest
      bottomRight = last rest
      action | isToggle = Toggle
             | isTurnOn = SwitchOn
             | isTurnOff = SwitchOff
      createPoint s = Point (read $ takeWhile (/=',') s) (read $ tail $ dropWhile (/=',') s)
  in
    Transform action $ Square (createPoint topLeft) (createPoint bottomRight)

performTransform :: Point -> State -> Transform -> State
performTransform point state (Transform action square)
  | point `within` square = performAction action state
  | otherwise = state

performTransforms :: [Transform] -> Point -> State
performTransforms ts p = foldl (performTransform p) Off ts

performTransformsOnPoints :: [Transform] -> [Point] -> [State]
performTransformsOnPoints ts = map (performTransforms ts)

points :: [Point]
points = [Point x y | x <- [0..999], y <- [0..999]]

countOnStates :: [State] -> Int
countOnStates = length . filter (==On)

solve :: String -> Int
solve input = countOnStates $ performTransformsOnPoints (map createTransform $ lines input) points

main = interact $ show . solve
