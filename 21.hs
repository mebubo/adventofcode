import Data.Function
import Data.Ord
import Data.List

data Item = Item { name   :: String
                 , itemCost :: Int
                 , itemDamage :: Int
                 , itemArmor  :: Int} deriving Show

weapons :: [Item]
weapons =
  [ Item "Dagger"        8     4       0
  , Item "Shortsword"   10     5       0
  , Item "Warhammer"    25     6       0
  , Item "Longsword"    40     7       0
  , Item "Greataxe"     74     8       0
  ]

armors :: [Item]
armors =
  [ Item "Leather"      13     0       1
  , Item "Chainmail"    31     0       2
  , Item "Splintmail"   53     0       3
  , Item "Bandedmail"   75     0       4
  , Item "Platemail"   102     0       5
  ]

rings :: [Item]
rings =
  [ Item "Damage +1"    25     1       0
  , Item "Damage +2"    50     2       0
  , Item "Damage +3"   100     3       0
  , Item "Defense +1"   20     0       1
  , Item "Defense +2"   40     0       2
  , Item "Defense +3"   80     0       3
  ]

data Fighter = Fighter { hp :: Int, damage :: Int, armor :: Int } deriving Show

createFighter :: Int -> [Item] -> Fighter
createFighter hp items = Fighter hp (sum $ map itemDamage items) (sum $ map itemArmor items)

readFighter :: [String] -> Fighter
readFighter ss =
        let nums = map (read . tail . dropWhile (/=':')) ss
            [hp, damage, armor] = nums
  in Fighter hp damage armor

choicesOfN :: Int -> [a] -> [[a]]
choicesOfN 0 _ = [[]]
choicesOfN _ [] = []
choicesOfN n (x:xs) = choicesOfN n xs ++ map (x:) (choicesOfN (n-1) xs)

choicesOfLength :: [Int] -> [a] -> [[a]]
choicesOfLength ns xs = concatMap (`choicesOfN` xs) ns

combineChoices :: [[[a]]] -> [[a]]
combineChoices = map (foldl1 (++)) . sequence

allItemChoices :: [[Item]]
allItemChoices = combineChoices [ choicesOfLength [1] weapons
                                , choicesOfLength [0, 1] armors
                                , choicesOfLength [0..2] rings]

outcome :: Fighter -> Fighter -> Bool
outcome boss player = (==0) . (`mod` 2) . length . takeWhile ((>0) . hp) $ fight boss player

blow :: Fighter -> Fighter -> Fighter
blow attacked attacker = Fighter adjustedRemainingHp (damage attacked) (armor attacked)
    where dealt = damage attacker - armor attacked
          adjustedRemainingHp = hp attacked - if dealt > 0 then dealt else 1

fight :: Fighter -> Fighter -> [Fighter]
fight a b = a : fight b (blow a b)

setOfItemsCost :: [Item] -> Int
setOfItemsCost = sum . map itemCost

findCheapestWinningItemSet :: Int -> Fighter -> [[Item]] -> [Item]
findCheapestWinningItemSet hp' boss = minimumBy (compare `on` setOfItemsCost) . filter (outcome boss . createFighter hp')

findMostExpensiveLosingItemSet :: Int -> Fighter -> [[Item]] -> [Item]
findMostExpensiveLosingItemSet hp' boss = maximumBy (compare `on` setOfItemsCost) . filter (not . outcome boss . createFighter hp')

main = do
    input <- getContents
    let boss = readFighter $ lines input
    print $ setOfItemsCost $ findCheapestWinningItemSet 100 boss allItemChoices
    print $ setOfItemsCost $ findMostExpensiveLosingItemSet 100 boss allItemChoices
