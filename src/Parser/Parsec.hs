{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Parser.Parsec
  ( type Parser (..),
    satisfy,
    char,
    digit,
    digits,
    newline,
    string,
    space,
    spaces,
    symbol,
    anySymbol,
    between,
    betweenSymbols,
    brackets,
    braces,
    parens,
    ignoreSpaces,
    letter,
    letters,
    eof,
    lookahead,
    type Result (..),
    type Error (..),
    type Input (..),
    chainl1,
    manyTill,
    someTill,
    try,
    spaceNoNewline,
    spacesNoNewline,
    many,
    some,
    optional,
  )
where

import Control.Applicative (many, optional, some, type Alternative (empty, (<|>)))
import Data.Char (isDigit, isLetter, isSpace)
import Data.Kind (type Type)
import Data.String (fromString)
import Parser.Error (type Error (type EndOfInput, type Unexpected))
import Parser.Input (type Input (head, null, uncons, unpack, type Token))
import Parser.Result (type Result (type Errors, type Result))

type Parser :: Type -> Type -> Type -> Type
newtype Parser i e o = Parser {runParser :: i -> Result i e (i, o)}

instance Functor (Parser i e) where
  fmap :: (a -> b) -> Parser i e a -> Parser i e b
  fmap f parser = Parser $ \input -> do
    (rest, x) <- runParser parser input
    Result (rest, f x)

instance Applicative (Parser i e) where
  pure :: a -> Parser i e a
  pure x = Parser $ pure . (,x)

  (<*>) :: Parser i e (a -> b) -> Parser i e a -> Parser i e b
  (<*>) pf po = Parser $ \input -> do
    (rest, f) <- runParser pf input
    (rest', x) <- runParser po rest
    Result (rest', f x)

instance Alternative (Parser i e) where
  empty :: Parser i e a
  empty = Parser $ const empty

  (<|>) :: Parser i e a -> Parser i e a -> Parser i e a
  (<|>) p1 p2 = Parser $ \input -> case runParser p1 input of
    Errors _ -> runParser p2 input
    Result res -> Result res

instance Monad (Parser i e) where
  return :: a -> Parser i e a
  return = pure

  (>>=) :: Parser i e a -> (a -> Parser i e b) -> Parser i e b
  (>>=) parser f = Parser $ \input -> case runParser parser input of
    Errors errs -> Errors errs
    Result (rest, x) -> runParser (f x) rest

instance (Semigroup a) => Semigroup (Parser i e a) where
  (<>) :: Parser i e a -> Parser i e a -> Parser i e a
  (<>) = liftA2 (<>)

instance (Semigroup a) => Monoid (Parser i e a) where
  mempty :: Parser i e a
  mempty = empty

satisfy :: (Input i, Token i ~ a) => (a -> Bool) -> Parser i e a
satisfy predicate = Parser $ \input -> case uncons input of
  Just (x, xs) | predicate x -> Result (xs, x)
  Just _ -> Errors [Unexpected input]
  Nothing -> Errors [EndOfInput]
{-# INLINEABLE satisfy #-}

char :: (Input i, Token i ~ Char) => Char -> Parser i e Char
char = satisfy . (==)
{-# INLINEABLE char #-}

digit :: (Input i, Token i ~ Char) => Parser i e Char
digit = satisfy isDigit
{-# INLINEABLE digit #-}

digits :: (Input i, Token i ~ Char) => Parser i e i
digits = fromString <$> some digit
{-# INLINEABLE digits #-}

string :: (Input i, Token i ~ Char) => i -> Parser i e i
string t = fromString <$> traverse char (unpack t)
{-# INLINEABLE string #-}

spaceNoNewline :: (Input i, Token i ~ Char) => Parser i e Char
spaceNoNewline = satisfy $ \c -> isSpace c && c /= '\n'
{-# INLINEABLE spaceNoNewline #-}

spacesNoNewline :: (Input i, Token i ~ Char) => Parser i e i
spacesNoNewline = fromString <$> many spaceNoNewline
{-# INLINEABLE spacesNoNewline #-}

space :: (Input i, Token i ~ Char) => Parser i e Char
space = satisfy isSpace
{-# INLINEABLE space #-}

spaces :: (Input i, Token i ~ Char) => Parser i e i
spaces = fromString <$> many space
{-# INLINEABLE spaces #-}

letter :: (Input i, Token i ~ Char) => Parser i e Char
letter = satisfy isLetter
{-# INLINEABLE letter #-}

letters :: (Input i, Token i ~ Char) => Parser i e i
letters = fromString <$> some letter
{-# INLINEABLE letters #-}

symbol :: (Input i, Token i ~ Char) => i -> Parser i e i
symbol = ignoreSpaces . string
{-# INLINEABLE symbol #-}

anySymbol :: (Input i, Token i ~ Char) => [i] -> Parser i e i
anySymbol = foldr1 (<|>) . fmap symbol
{-# INLINEABLE anySymbol #-}

between :: Parser i e o -> Parser i e o' -> Parser i e o'' -> Parser i e o''
between open close p = open *> p <* close
{-# INLINEABLE between #-}

betweenSymbols :: (Input i, Token i ~ Char) => i -> i -> Parser i e a -> Parser i e a
betweenSymbols open close = between (symbol open) (symbol close)
{-# INLINEABLE betweenSymbols #-}

brackets :: (Input i, Token i ~ Char) => Parser i e a -> Parser i e a
brackets = betweenSymbols "[" "]"
{-# INLINEABLE brackets #-}

braces :: (Input i, Token i ~ Char) => Parser i e a -> Parser i e a
braces = betweenSymbols "{" "}"
{-# INLINEABLE braces #-}

parens :: (Input i, Token i ~ Char) => Parser i e a -> Parser i e a
parens = betweenSymbols "(" ")"
{-# INLINEABLE parens #-}

ignoreSpaces :: (Input i, Token i ~ Char) => Parser i e a -> Parser i e a
ignoreSpaces = between spaces spaces
{-# INLINEABLE ignoreSpaces #-}

eof :: (Input i) => Parser i e ()
eof = Parser $ \case
  input | Parser.Input.null input -> Result (input, ())
  _ -> Errors [EndOfInput]
{-# INLINEABLE eof #-}

lookahead :: (Input i, Token i ~ Char) => Parser i i (Maybe Char)
lookahead = Parser $ \case
  input
    | Parser.Input.null input -> Errors [EndOfInput]
    | otherwise -> Result (input, Parser.Input.head input)
{-# INLINEABLE lookahead #-}

chainl1 :: Parser i e a -> Parser i e (a -> a -> a) -> Parser i e a
chainl1 p op = Parser $ \input ->
  case runParser p input of
    Errors errs -> Errors errs
    Result (rest, x) ->
      let go x' input' = case runParser op input' of
            Errors _ -> Result (input', x')
            Result (rest', f) ->
              case runParser p rest' of
                Errors errs' -> Errors errs'
                Result (rest'', y) -> go (f x' y) rest''
       in go x rest

manyTill :: Parser i e a -> Parser i e b -> Parser i e [a]
manyTill p end = go
  where
    go = ([] <$ end) <|> ((:) <$> p <*> go)

try :: Parser i e a -> Parser i e a
try p = Parser $ \input -> case runParser p input of
  Errors _ -> Errors []
  res -> res

someTill :: Parser i e a -> Parser i e b -> Parser i e [a]
someTill p end = (:) <$> p <*> manyTill p end
{-# INLINEABLE someTill #-}

newline :: (Input i, Token i ~ Char) => Parser i e Char
newline = char '\n'
{-# INLINEABLE newline #-}
