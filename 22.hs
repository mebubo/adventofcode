data Player = Player { playerHp :: Int, mana :: Int, playerSpells :: [Spell] } deriving Show
data Boss = Boss { bossHp :: Int, damage :: Int } deriving Show

data Spell = MagicMissile Int Int
           | Drain Int Int Int
           | Shield Int Int Int
           | Poison Int Int Int
           | Recharge Int Int Int deriving Show

spells :: [Spell]
spells = [ MagicMissile 53 4
         , Drain 73 2 2
         , Shield 113 6 7
         , Poison 173 6 3
         , Recharge 229 5 101
         ]

player :: Player
player = Player 50 500 []

boss :: Boss
boss = Boss 51 9
