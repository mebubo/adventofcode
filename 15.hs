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

recipeScore :: (Ingredient -> Ingredient) -> [Ingredient] -> Recipe -> Int
recipeScore ingredientsFilter ingredients recipe = product . map sumIngredient . transpose $ map ingredientsFilter ingredients
  where
    sumIngredient = zeroIfNegative . sum . zipWith (*) recipe
    zeroIfNegative x = if x < 0 then 0 else x

recipeSize = 100

type Scoring = [Ingredient] -> [Recipe] -> [Int]

scores1 :: Scoring
scores1 ingredients = map (recipeScore limit ingredients)
  where
    limit i = take (length i - 1) i
    
scores2 :: Scoring
scores2 ingredients recipes = scores1 ingredients $ filter (calories 500) recipes
  where
    calories :: Int -> Recipe -> Bool
    calories n r = n == recipeScore ((:[]) . last) ingredients r

solution :: Scoring -> [Ingredient] -> [Recipe] -> Int
solution scores ingredients = maximum . scores ingredients

main = do
  input <- getContents
  let ingredients = map readIngredient $ lines input
      recipes = allRecipes (length ingredients) recipeSize
  print $ solution scores1 ingredients recipes
  print $ solution scores2 ingredients recipes
