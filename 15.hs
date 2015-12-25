module Day15 where

import Data.Set (fromList, toList)
import Data.List (permutations, transpose)
import Text.Regex.Posix

applyToEach :: (Int -> Int) -> [Int] -> [[Int]]
applyToEach _ [] = []
applyToEach f (x:xs) = (f x:xs) : map (x:) (applyToEach f xs)

uniq :: Ord a => [a] -> [a]
uniq = toList . fromList

splitSum :: Int -> Int -> [[Int]]
splitSum n x | n < 0 || x < 0 = error "Negative arguments"
splitSum n 0 = [replicate n 0]
splitSum n x =
  let sub = splitSum n (x-1)
  in uniq . concatMap (applyToEach (+1)) $ sub

nPartition :: Int -> Int -> [[Int]]
nPartition 1 x = [[x]]
nPartition n x = [y:rest | y <- [0..x], rest <- nPartition (n-1) (x-y)]

type Ingredient = [Int]
type Recipe = [Int]

allRecipes :: Int -> Int -> [Recipe]
allRecipes n = uniq . concatMap permutations . nPartition n

readIngredient :: String -> [Int]
readIngredient xs = map read . getAllTextMatches $ xs =~ "-?[[:digit:]]+"

recipeScore :: Int -> [Ingredient] -> Recipe -> Int
recipeScore n ingredients recipe = product . map sumIngredient . transpose $ limit ingredients
  where
    sumIngredient = zeroIfNegative . sum . zipWith (*) recipe
    zeroIfNegative x = if x < 0 then 0 else x
    limit = map $ take n

recipeSize = 100

solution :: Int -> [Ingredient] -> Int
solution n ingredients = maximum . map (recipeScore n ingredients) $ allRecipes (length ingredients) recipeSize

main = do
  input <- getContents
  print $ solution 4 $ map readIngredient $ lines input
