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
choicesOfN n (x:xs) = choicesOfN n xs ++ (map (x:) $ choicesOfN (n-1) xs)

choicesOfLength :: [Int] -> [a] -> [[a]]
choicesOfLength ns xs = concatMap (\n -> choicesOfN n xs) ns

combineChoices :: [[[a]]] -> [[a]]
combineChoices = map (foldl1 (++)) . sequence

allItemChoices = combineChoices [choicesOfLength [1] weapons
                                , choicesOfLength [0, 1] armors
                                , choicesOfLength [0..2] rings]
