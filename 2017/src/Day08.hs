module Day08 where

import Text.Megaparsec
import Text.Megaparsec.Char
-- import Control.Applicative (many, some)
import Data.Maybe (maybeToList)
import Prelude hiding (LT, GT, EQ)
import qualified Control.Monad.State as S
import qualified Data.Map as M
import Data.Foldable (traverse_)

data Instruction = Instruction Update Condition deriving Show

data Update = Update Name IncDec Value deriving Show
data Condition = Condition Name Operator Value deriving Show

type Name = String
type Value = Int

data IncDec = Inc | Dec deriving (Eq, Show)
data Operator
    = LT
    | GT
    | LE
    | GE
    | NE
    | EQ deriving Show

type Parser = Parsec () String

name :: Parser Name
name = some alphaNumChar

value :: Parser Value
value = read <$> int
    where
        int :: Parser String
        int = (++) <$> sign <*> some digitChar
        sign = maybeToList <$> optional (char '-')

literal :: String -> a -> Parser a
literal s a = string s *> pure a

incdec :: Parser IncDec
incdec = literal "inc" Inc <|> literal "dec" Dec

operator :: Parser Operator
operator =
    literal "<=" LE <|>
    literal ">=" GE <|>
    literal "!=" NE <|>
    literal "==" EQ <|>
    literal "<" LT <|>
    literal ">" GT

condition :: Parser Condition
condition = Condition <$>
    (string "if " *> name <* space1) <*>
    operator <* space1 <*>
    value

update :: Parser Update
update = Update <$> name <* space1 <*> incdec <* space1 <*> value

instruction :: Parser Instruction
instruction = Instruction <$> (update <* space1) <*> condition <* char '\n'

instructions :: Parser [Instruction]
instructions = many instruction <* eof

type St = S.State (M.Map Name Value)

performUpdate :: Update -> St ()
performUpdate (Update n i v) | i == Inc = upd v
                             | i == Dec = upd (-v)
    where
        upd :: Value -> St ()
        upd x = S.modify $ M.insertWith (+) n x

evalCondition :: Condition -> St Bool
evalCondition (Condition n o v) = do
    m <- S.get
    return $ val m `op` v
        where
            val = M.findWithDefault 0 n
            op = case o of
                LE -> (<=)
                GE -> (>=)
                NE -> (/=)
                EQ -> (==)
                LT -> (<)
                GT -> (>)

execInstruction :: Instruction -> St Value
execInstruction (Instruction u c) = (evalCondition c >>= f) >> maxValue
    where
        f True = performUpdate u
        f False = pure ()

execInstructions :: [Instruction] -> St [Value]
execInstructions = traverse execInstruction

maxValue :: St Value
maxValue = maximum <$> S.get

main :: IO ()
main = do
    c <- readFile "input/08.input"
    let Right is = parse instructions "" c
        maxes = S.evalState (execInstructions is) M.empty
    print $ last maxes
    print $ maximum maxes
