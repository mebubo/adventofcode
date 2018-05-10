{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = Parser {
        runParser :: String -> Maybe (a, String)
    }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f m1 =
        m1 >>= \x1 -> return (f x1)

instance Applicative Parser where
    pure = return

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    m1 <*> m2 =
        m1 >>= \x1 ->
            m2 >>= \x2 -> return (x1 x2)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser (const Nothing)

    (<|>) :: Parser a -> Parser a -> Parser a
    Parser a <|> Parser b = Parser $ liftA2 (<|>) a b

instance Monad Parser where
    return :: a -> Parser a
    return a = Parser (\s -> Just (a, s))

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser fa) >>= g = Parser $ \s ->
        case fa s of
            Nothing -> Nothing
            Just (a, s') -> runParser (g a) s'

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s ->
    case s of
        [] -> Nothing
        (x:xs) -> if p x then Just (x, xs) else Nothing

char :: Char -> Parser Char
char c = satisfy (==c)

int :: Parser Int
int = fmap read $ some (satisfy isDigit)

marker :: Parser (Int, Int)
marker = f <$> char '(' <*> int <*> char 'x' <*> int <*> char ')'
    where f _ a _ b _ = (a, b)

text :: Parser Entry
text = fmap S $ some $ satisfy (/='(')

len :: Int -> Parser String
len n = replicateM n $ satisfy $ const True

marked :: Parser Entry
marked = marker >>= \(a, b) -> M <$> pure a <*> pure b <*> len a

entry :: Parser Entry
entry = text <|> marked

entries :: Parser [Entry]
entries = many entry

data Entry = S String | M Int Int String deriving Show

entryToString :: Entry -> String
entryToString (S s) = s
entryToString (M _ b s) = concat $ replicate b s

main :: IO ()
main = do
    input <- getContents
    let stripped = filter (/='\n') input
    let es = runParser entries stripped
    print fmap (length . concat . map entryToString . fst) es

