import Text.Show.Functions

data SpellItem = SpellItem Int Int SpellKind deriving Show

data SpellKind = MagicMissile Int
               | Drain Int
               | Shield Int
               | Poison Int
               | Recharge Int deriving Show

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

data State = State { hp :: Int, mana :: Int, bossHp:: Int, spentMana :: Int, turnNumber :: Int, effects :: [Effect], spellsCast :: [SpellKind]} deriving Show

data Effect = Effect SpellKind Int (State -> State) deriving Show

type Spell = (State -> State)

createSpell :: SpellKind -> Spell
createSpell (MagicMissile x) = \s -> s { bossHp = (bossHp s) - x }
createSpell (Drain x) = \s -> s { hp = (hp s) + x, bossHp = (bossHp s) - x }
createSpell (Shield x) = id
createSpell (Poison x) = \s -> s { bossHp = (bossHp s) - x }
createSpell (Recharge x) = \s -> s { mana = (mana s) + x }

createEffect :: SpellItem -> State -> State
createEffect (SpellItem t c k) s = s {mana = (mana s) - c,
                                      spentMana = (spentMana s) + c,
                                      effects = Effect k t (createSpell k) : (effects s)}

bossTurn :: State -> State
bossTurn s = s { turnNumber = (turnNumber s) + 1,
                 hp = (hp s) - bossDamage }
             where diff = damage - (armor s)
                   bossDamage = if diff < 1 then 1 else diff

playerTurn :: SpellItem -> State -> State
playerTurn i s =
  let newState = createEffect i s
  in newState {turnNumber = (turnNumber s) + 1
              , spellsCast = kindFromItem i : spellsCast s}

applyEffect :: Effect -> State -> State
applyEffect (Effect _ _ f) = f

applyEffects :: [Effect] -> State -> State
applyEffects [] s = s
applyEffects (e:es) s = applyEffects es (applyEffect e s)

applyAllEffects :: State -> State
applyAllEffects s = applyEffects (effects s) s

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
playerTurns s = [playerTurn i s | i <- spells, not $ any (sameSpell (kindFromItem i)) (map kindFromEffect (effects s)), canAfford i]
                where kindFromEffect (Effect k _ _) = k
                      canAfford (SpellItem _ c _) = (mana s) >= c

turns :: State -> [State]
turns s = if isPlayerTurn then playerTurns s else [bossTurn s]
  where isPlayerTurn = (turnNumber s) `mod` 2 == 0

playerWon :: State -> Bool
playerWon s = (bossHp s) <= 0

bossWon :: State -> Bool
bossWon s = (hp s) <= 0

gameOver :: State -> Bool
gameOver s = bossWon s || playerWon s

step :: [State] -> [State]
step ss = concatMap turnIfNotOver $ map applyIfNotOver ss
  where turnIfNotOver s = if gameOver s then [s] else turns s
        applyIfNotOver s = if gameOver s then s else allEffects s

game :: [State] -> [[State]]
game is = is : game (step is)

outcomes :: State -> [State]
-- outcomes s = head $ dropWhile (any (not . gameOver)) (game [s])
outcomes s = game [s] !! 20

result :: State -> Int
result = minimum . map spentMana . filter playerWon . outcomes

damage = 9

main = do
  print $ result $ State 50 500 51 0 0 [] []


