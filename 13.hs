module Day13 where

import Data.List (permutations)
import Data.Map ((!), fromList, Map, keys)
import qualified Data.Set as S 

readPreference :: String -> ((String, String), Int)
readPreference = 
  let r [n1,_,sign,x,_,_,_,_,_,_,n2] = ((n1, n2), toSignedInt sign x)
      toSignedInt "lose" x = - (read x)
      toSignedInt "gain" x = read x
  in r . words . filter (/= '.')

neighbours :: [a] -> [(a, a)]
neighbours xs = zip xs ((tail xs) ++ [head xs])

circularPermutations :: [a] -> [[a]]
circularPermutations (x:xs) = map (x:) $ permutations xs

type Prefs = Map (String, String) Int

pairCost :: Prefs -> (String, String) -> Int
pairCost p (a, b) = p `lookupPref` (a, b) + p `lookupPref` (b, a)

lookupPref :: Prefs -> (String, String) -> Int
lookupPref p (a, b) | a == "me" || b == "me" = 0
                | otherwise = p ! (a, b)

arrangementCost :: Prefs -> [String] -> Int
arrangementCost p = sum . map (pairCost p) . neighbours

guests :: Prefs -> [String]
guests = S.toList . S.fromList . map fst . keys

solution :: [String] -> Prefs -> Int
solution gs prefs = maximum . map (arrangementCost prefs) . circularPermutations $ gs

readPrefs :: [String] -> Prefs
readPrefs = fromList . map readPreference

main = do
  input <- getContents
  let prefs = readPrefs $ lines input
  print $ solution (guests prefs) prefs
  print $ solution ("me":guests prefs) prefs
