import Text.Show.Functions()

data SpellItem = SpellItem Int Int SpellKind deriving Show

data SpellKind = MagicMissile Int
               | Drain Int
               | Shield Int
               | Poison Int
               | Recharge Int deriving Show

kindFromItem :: SpellItem -> SpellKind
kindFromItem (SpellItem _ _ k) = k

sameSpell :: SpellKind -> SpellKind -> Bool
sameSpell (MagicMissile _) (MagicMissile _) = True
sameSpell (Drain _) (Drain _) = True
sameSpell (Shield _) (Shield _) = True
sameSpell (Poison _) (Poison _) = True
sameSpell (Recharge _) (Recharge _) = True
sameSpell _ _ = False

spells :: [SpellItem]
spells = [ SpellItem 1 53 $ MagicMissile 4
         , SpellItem 1 73 $ Drain 2
         , SpellItem 6 113 $ Shield 6
         , SpellItem 6 173 $ Poison 3
         , SpellItem 5 229 $ Recharge 101
         ]

data State = State { hp :: Int
                   , mana :: Int
                   , bossHp:: Int
                   , spentMana :: Int
                   , turnNumber :: Int
                   , effects :: [Effect]
                   , spellsCast :: [SpellKind]
                   } deriving Show

data Effect = Effect SpellKind Int (State -> State) deriving Show

type Spell = (State -> State)

createSpell :: SpellKind -> Spell
createSpell (MagicMissile x) = \s -> s { bossHp = bossHp s - x }
createSpell (Drain x) = \s -> s { hp = hp s + x, bossHp = bossHp s - x }
createSpell (Shield _) = id
createSpell (Poison x) = \s -> s { bossHp = bossHp s - x }
createSpell (Recharge x) = \s -> s { mana = mana s + x }

createEffect :: SpellItem -> State -> State
createEffect (SpellItem t c k) s = s {mana = mana s - c,
                                      spentMana = spentMana s + c,
                                      effects = Effect k t (createSpell k) : effects s}

bossTurn :: Bool -> State -> State
bossTurn hard s = s { turnNumber = turnNumber s + 1,
                 hp = hp s - bossDamage - hardDamage }
             where diff = damage - armor s
                   bossDamage = if diff < 1 then 1 else diff
                   hardDamage = if hard then 1 else 0

playerTurn :: SpellItem -> State -> State
playerTurn i s =
  let newState = createEffect i s
  in newState {turnNumber = turnNumber s + 1
              , spellsCast = kindFromItem i : spellsCast s}

applyEffect :: State -> Effect -> State
applyEffect s (Effect _ _ f) = f s

applyEffects :: State -> [Effect] -> State
applyEffects = foldl applyEffect

applyAllEffects :: State -> State
applyAllEffects s = applyEffects s (effects s)

expireEffect :: Effect -> Effect
expireEffect (Effect k t f) = Effect k (t-1) f

expireEffects :: [Effect] -> [Effect]
expireEffects = map expireEffect

pruneEffects :: [Effect] -> [Effect]
pruneEffects = filter notExpired
               where notExpired (Effect _ t _) = t > 0

allEffects :: State -> State
allEffects s =
  let newState = applyAllEffects s
      newEffects = pruneEffects . expireEffects $ effects newState
  in
    newState {effects = newEffects}

armor :: State -> Int
armor s = if shieldIsInEffects (effects s) then 7 else 0
  where
    shieldIsInEffects :: [Effect] -> Bool
    shieldIsInEffects = any isShield
    isShield :: Effect -> Bool
    isShield (Effect (Shield _) _ _) = True
    isShield _ = False

playerTurns :: State -> [State]
playerTurns s = [playerTurn i s
                | i <- spells
                , not $ any (sameSpell (kindFromItem i)) (map kindFromEffect (effects s))
                , canAfford i]
                where kindFromEffect (Effect k _ _) = k
                      canAfford (SpellItem _ c _) = mana s >= c

turns :: Bool -> State -> [State]
turns hard s = if isPlayerTurn then playerTurns s else [bossTurn hard s]
  where isPlayerTurn = turnNumber s `mod` 2 == 0

playerWon :: State -> Bool
playerWon s = bossHp s <= 0

bossWon :: State -> Bool
bossWon s = hp s <= 0

gameOver :: State -> Bool
gameOver s = bossWon s || playerWon s

step :: Bool -> [State] -> [State]
step hard = concatMap (turnIfNotOver . applyIfNotOver)
  where turnIfNotOver s = if gameOver s then [s] else turns hard s
        applyIfNotOver s = if gameOver s then s else allEffects s

type Game = [State] -> [[State]]

game :: Game
game is = is : game (step False is)

hardGame :: Game
hardGame is = is : hardGame (step True is)

outcomes :: Game -> State -> [State]
-- outcomes game s = head $ dropWhile (any (not . gameOver)) (game [s])
outcomes g s = g [s] !! 20

result :: Game -> State -> Int
result g = minimum . map spentMana . filter playerWon . outcomes g

damage :: Int
damage = 9

main :: IO ()
main = do
  print $ result game $ State 50 500 51 0 0 [] []
  print $ result hardGame $ State 49 500 51 0 0 [] []
