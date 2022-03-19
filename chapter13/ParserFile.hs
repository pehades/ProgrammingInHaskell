module ParserFile (
Parser, parse, item, sat, digit, lower, upper, char, string,
ident, space, nat, int, many, some, token, identifier, natural,
integer, symbol, nats) where

import Data.Char
import Data.List
import Control.Applicative

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> (String -> [(a, String)])
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                  []     -> []
                  (x:xs) -> [(x, xs)])


-- Functor

instance Functor Parser where
  fmap g p = P (\inp -> case parse p inp of
                          [] -> []
                          [(x, string)] -> [(g x, string)])


-- Applicative

instance Applicative Parser where
  pure x = P (\inp -> [(x, inp)])

  -- <*> :: P(a->b) -> P a -> P b
  pg <*> px = P (\inp -> case parse pg inp of
                           [] -> []
                           [(g, string)] -> parse (fmap g px) string)


--three :: Parser (Char, Char)
--three = pure g <*> item <*> item <*> item
--        where g x y z = (x, z)

instance Monad Parser where
  return = pure

  px >>= g = P (\inp -> case parse px inp of
                          [] -> []
                          [(x, string)] -> parse (g x) string)

three :: Parser (Char, Char)
three = do x <- item
           item
           z <- item
           return (x, z)

-- for pedagogical reasons
--three = item >>= \x ->
--        item >>= \y ->
--        item >>= \z ->
--        return (x, z)

instance Alternative Parser where
  empty = P (\inp -> [])

  p <|> q = P (\inp -> case parse p inp of
                         [] -> parse q inp
                         x -> x)


-- Derived Primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat (isDigit)

lower :: Parser Char
lower = sat (isLower)

upper :: Parser Char
upper = sat (isUpper)

letter :: Parser Char
letter = sat (isAlpha)

alphanum :: Parser Char
alphanum = sat (isAlphaNum)

isChar :: Char -> (Char -> Bool)
isChar a = ( == a)

char :: Char -> Parser Char
char a = sat ( ==a)

string :: String -> Parser String
string [] = return []
string (x:xs) = do a <- char x
                   b <- string xs
                   return (x:xs)

ident :: Parser String
ident = do xs <- many alphanum
           return xs

space :: Parser ()
space = do many (sat (isChar ' '))
           return ()

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do (char '-')
         n <- nat
         return (-n)
         <|> nat

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)