data Action = SwitchOff | SwitchOn | Toggle deriving Show

data Point = Point Int Int deriving Show

data Square = Square Point Point deriving Show

within :: Point -> Square -> Bool
within (Point x y) (Square (Point a b) (Point c d)) = a <= x && x <= c && b <= y && y <= d

data Transform = Transform Action Square deriving Show

type State = Int

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

initialState = 0

performTransforms :: [Transform] -> Point -> State
performTransforms ts p = foldl (performTransform p) initialState ts

performTransformsOnPoints :: [Transform] -> [Point] -> [State]
performTransformsOnPoints ts = map (performTransforms ts)

points :: [Point]
points = [Point x y | x <- [0..999], y <- [0..999]]

countOnStates :: [State] -> Int
countOnStates = sum

solve :: [String] -> Int
solve input = countOnStates $ performTransformsOnPoints (map createTransform input) points

performAction1 :: Action -> State -> State
performAction1 SwitchOff _ = 0
performAction1 SwitchOn _ = 1
performAction1 Toggle 1 = 0
performAction1 Toggle 0 = 1

performAction2 :: Action -> State -> State
performAction2 SwitchOff s = if s == 0 then 0 else s - 1
performAction2 SwitchOn s = s + 1
performAction2 Toggle s = s + 2

performAction = performAction2

main = interact $ show . solve . lines
