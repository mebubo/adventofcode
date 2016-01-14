data Player = Player { playerHp :: Int, mana :: Int, playerSpells :: [Spell] } deriving Show
data Boss = Boss { bossHp :: Int, damage :: Int } deriving Show

data Spell = MagicMissile Int Int
           | Drain Int Int Int
           | Shield Int Int
           | Poison Int Int
           | Recharge Int Int deriving Show

data TimedSpell = TimedSpell Int Spell

spells :: [TimedSpell]
spells = [ TimedSpell 1 $ MagicMissile 53 4
         , TimedSpell 1 $ Drain 73 2 2
         , TimedSpell 6 $ Shield 113 7
         , TimedSpell 6 $ Poison 173 3
         , TimedSpell 5 $ Recharge 229 101
         ]

player :: Player
player = Player 50 500 []

boss :: Boss
boss = Boss 51 9

applySpell :: (Player -> Boss -> TimedSpell) -> (Player, Boss, TimedSpell)
applySpell (p, Boss hp damage, TimedSpell t (MagicMissile m d)) = (p, Boss (hp - d) damage, TimedSpell (t-1) $ MagicMissile m d)
