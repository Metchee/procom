{-
-- EPITECH PROJECT, 2025
-- procom
-- File description:
-- Core
-}

module Parser.Core where

import Data.Char (isDigit, isAlpha, isSpace)
import Data.List (isPrefixOf)
import Control.Applicative (Alternative, empty, (<|>), many, some)

newtype Parser a = Parser {
  runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (result, rest) <- p input
    return (f result, rest)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (a, input)
  Parser pf <*> Parser pa = Parser $ \input -> do
    (f, input') <- pf input
    (a, input'') <- pa input'
    return (f a, input'')

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser $ \input -> do
    (a, input') <- p input
    runParser (f a) input'

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \input ->
    p1 input <|> p2 input

item :: Parser Char
item = Parser $ \input -> case input of
  []     -> Nothing
  (x:xs) -> Just (x, xs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \input -> do
  (x, rest) <- runParser item input
  if pred x then Just (x, rest) else Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs

spaces :: Parser String
spaces = many (satisfy isSpace)

token :: Parser a -> Parser a
token p = do
  spaces
  a <- p
  spaces
  return a

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha

alphaNum :: Parser Char
alphaNum = letter <|> digit <|> satisfy (== '_')

int :: Parser Int
int = fmap read (some digit)

quotedString :: Parser String
quotedString = do
  char '"'
  s <- many (satisfy (/= '"'))
  char '"'
  return s

tillString :: String -> Parser String
tillString s = Parser $ \input ->
  let helper acc "" = Just (reverse acc, "")
      helper acc i@(c:cs)
        | s `isPrefixOf` i = Just (reverse acc, i)
        | otherwise = helper (c:acc) cs
  in helper [] input

peek :: Parser a -> Parser Bool
peek p = Parser $ \input -> case runParser p input of
  Just _  -> Just (True, input)
  Nothing -> Just (False, input)
