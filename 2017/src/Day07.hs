module Day07 where

import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Prim (parse)
import Text.Parsec.Char (anyChar, char, digit)
import Text.ParserCombinators.Parsec (many, many1, sepBy, eof)
import Text.ParserCombinators.Parsec.Char (alphaNum)
import Data.List (find, sortOn)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Either (fromRight)
import Data.Tree (unfoldTree, Tree, subForest, rootLabel)
import Control.Applicative ((<|>))

type Input = [Program]

type Name = String
type Weight = Int

data Program = Program
    { programName :: Name
    , programWeight :: Weight
    , programChildren :: [Name]
    } deriving Show

name :: Parser Name
name = many1 alphaNum

weight :: Parser Int
weight = do
    char '('
    i <- read <$> many1 digit
    char ')'
    return i

arrow :: Parser ()
arrow = do
    char '-'
    char '>'
    pure ()

above :: Parser [Name]
above = do
    whitespace
    arrow
    whitespace
    name `sepBy` (char ',' >> char ' ')

whitespace :: Parser Char
whitespace = char ' '

empty :: Parser [Name]
empty = pure []

program :: Parser Program
program = do
    n <- name
    whitespace
    w <- weight
    a <- above <|> empty
    char '\n'
    return $ Program n w a

programs :: Parser [Program]
programs = do
    result <- many program
    eof
    return result

readInput :: IO Input
readInput = do
    Right x <- parseFromFile programs "input/07.input"
    return x

bottom :: Input -> Name
bottom i = S.findMax $ bs `S.difference` as
    where
        as = S.fromList $ programChildren =<< i
        bs = S.fromList $ programName <$> i

type InputMap = M.Map Name (Weight, [Name])

buildMap :: Input -> InputMap
buildMap = M.fromList . (toPairs <$>)
    where
        toPairs = (,) <$> programName <*> v
        v = (,) <$> programWeight <*> programChildren

buildTree :: Input -> Tree Int
buildTree i = unfoldTree (buildMap i M.!) (bottom i)

-- https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day07.hs
findBad :: Tree Int -> Maybe Int
findBad t = listToMaybe badChildren <|> anomaly
    where
        sf = subForest t
        badChildren :: [Int]
        badChildren = mapMaybe findBad sf
        weightMap :: M.Map Int [Int]
        weightMap = M.fromListWith (++) . map g $ sf
        g :: Tree Int -> (Int, [Int])
        g t' = (sum t', [rootLabel t'])
        anomaly :: Maybe Int
        anomaly = case sortOn (length . snd) (M.toList weightMap) of
            [] -> Nothing
            [_] -> Nothing
            [(tot1, [w]), (tot2, _)] -> Just $ w + tot2 - tot1
            _ -> error "More than one anomaly"

main :: IO ()
main = do
    i <- readInput
    putStrLn $ bottom i
    print $ fromJust . findBad . buildTree $ i
