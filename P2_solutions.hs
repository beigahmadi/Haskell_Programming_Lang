-- Mahdi Beigahmadi
-- Student ID: 301570853
-- CMPT383, Programming Assignment 2
-- Feb 2025

module Main where

import Data.Char (isLower, isAlphaNum, isSpace)
import System.Environment (getArgs)
import Control.Applicative (many, (<|>), Alternative(..))

data Prop = Const Bool
          | Var String
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Iff Prop Prop
          deriving (Eq, Read, Show)

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f p = Parser $ \s -> case parse p s of
    Just (x, s') -> Just (f x, s')
    Nothing -> Nothing

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  pf <*> px = Parser $ \s -> case parse pf s of
    Just (f, s') -> parse (fmap f px) s'
    Nothing -> Nothing

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \s -> case parse p s of
    Just (x, s') -> parse (f x) s'
    Nothing -> Nothing

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  p <|> q = Parser $ \s -> case parse p s of
    Just res -> Just res
    Nothing -> parse q s

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \s -> case s of
  (c:cs) | pred c -> Just (c, cs)
  _ -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string []     = return []
string (c:cs) = do
  char c
  string cs
  return (c:cs)

space :: Parser ()
space = many (satisfy isSpace) >> return ()

token :: Parser a -> Parser a
token p = space >> p

constant :: Parser Prop
constant = (token (char 'T') >> return (Const True))
       <|> (token (char 'F') >> return (Const False))

ident :: Parser String
ident = do
  c  <- satisfy isLower
  cs <- many (satisfy isAlphaNum)
  return (c:cs)

var :: Parser Prop
var = Var <$> token ident

factor :: Parser Prop -> Parser Prop
factor formulaParser =
      (token (char '(') >> formulaParser <* token (char ')'))
  <|> constant
  <|> var

unaryTerm :: Parser Prop -> Parser Prop
unaryTerm formulaParser = do
  nots <- many (token (char '!'))
  p <- factor formulaParser
  return (foldr (\_ acc -> Not acc) p nots)

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = do
  x <- p
  rest x
  where
    rest x = (do f <- op
                 y <- chainr1 p op
                 return (f x y))
             <|> return x

andTerm :: Parser Prop -> Parser Prop
andTerm formulaParser =
  chainr1 (unaryTerm formulaParser) (token (string "/\\") >> return And)

orTerm :: Parser Prop -> Parser Prop
orTerm formulaParser =
  chainr1 (andTerm formulaParser) (token (string "\\/") >> return Or)

impTerm :: Parser Prop -> Parser Prop
impTerm formulaParser =
  chainr1 (orTerm formulaParser) (token (string "->") >> return Imply)

formula :: Parser Prop
formula = chainr1 (impTerm formula) (token (string "<->") >> return Iff)

parseFormula :: String -> String
parseFormula s = case parse (formula <* space) s of
  Just (v, "") -> show v
  _ -> "Parse Error"

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1 then
    putStrLn ""
  else do
    let file = head args
    content <- readFile file
    let formulas = filter (not . null) (lines content)
    mapM_ (putStrLn . parseFormula) formulas
