{-|
Module      : Parser.Parsec
Description : Exports the Parsec parser module.
-}

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

-- | The parser type.
type Parser :: Type -> Type -> Type -> Type
newtype Parser i e o = Parser {runParser :: i -> Result i e (i, o)}

-- | The 'Functor' instance for the parser type.
instance Functor (Parser i e) where
  -- | Maps a function over the result of the parser.
  fmap :: (a -> b) -> Parser i e a -> Parser i e b
  fmap f parser = Parser $ \input -> do
    (rest, x) <- runParser parser input
    Result (rest, f x)

-- | The 'Applicative' instance for the parser type.
instance Applicative (Parser i e) where
  -- | Lifts a value to a parser.
  pure :: a -> Parser i e a
  pure x = Parser $ pure . (,x)

  -- | Applies a function in the parser to a value in the parser.
  (<*>) :: Parser i e (a -> b) -> Parser i e a -> Parser i e b
  (<*>) pf po = Parser $ \input -> do
    (rest, f) <- runParser pf input
    (rest', x) <- runParser po rest
    Result (rest', f x)

-- | The 'Alternative' instance for the parser type.
instance Alternative (Parser i e) where
  -- | The empty parser.
  empty :: Parser i e a
  empty = Parser $ const empty

  -- | Choice of two parsers.
  (<|>) :: Parser i e a -> Parser i e a -> Parser i e a
  (<|>) p1 p2 = Parser $ \input -> case runParser p1 input of
    Errors _ -> runParser p2 input
    Result res -> Result res

-- | The 'Monad' instance for the parser type.
instance Monad (Parser i e) where
  -- | Returns a value in the parser.
  return :: a -> Parser i e a
  return = pure

  -- | Binds a parser to a function.
  (>>=) :: Parser i e a -> (a -> Parser i e b) -> Parser i e b
  (>>=) parser f = Parser $ \input -> case runParser parser input of
    Errors errs -> Errors errs
    Result (rest, x) -> runParser (f x) rest

-- | The 'Semigroup' instance for the parser type.
instance (Semigroup a) => Semigroup (Parser i e a) where
  -- | Combines two parsers.
  (<>) :: Parser i e a -> Parser i e a -> Parser i e a
  (<>) = liftA2 (<>)

-- | The 'Monoid' instance for the parser type.
instance (Semigroup a) => Monoid (Parser i e a) where
  -- | The empty parser.
  mempty :: Parser i e a
  mempty = empty

-- | The base predicate parser.
-- Succeeds if the predicate is true for the next token.
-- >>> runParser (satisfy (== 'a')) "abc"
-- Result ("bc",'a')
satisfy :: (Input i, Token i ~ a) => (a -> Bool) -> Parser i e a
satisfy predicate = Parser $ \input -> case uncons input of
  Just (x, xs) | predicate x -> Result (xs, x)
  Just _ -> Errors [Unexpected input]
  Nothing -> Errors [EndOfInput]
{-# INLINEABLE satisfy #-}

-- | Parses a character.
-- >>> runParser (char 'a') "abc"
-- Result ("bc",'a')
char :: (Input i, Token i ~ Char) => Char -> Parser i e Char
char = satisfy . (==)
{-# INLINEABLE char #-}

-- | Parses a digit.
-- >>> runParser digit "123"
-- Result ("23",'1')
digit :: (Input i, Token i ~ Char) => Parser i e Char
digit = satisfy isDigit
{-# INLINEABLE digit #-}

-- | Parses a sequence of digits.
-- >>> runParser digits "123"
-- Result ("","123")
digits :: (Input i, Token i ~ Char) => Parser i e i
digits = fromString <$> some digit
{-# INLINEABLE digits #-}

-- | Parses a string.
-- >>> runParser (string "abc") "abcdef"
-- Result ("def","abc")
string :: (Input i, Token i ~ Char) => i -> Parser i e i
string t = fromString <$> traverse char (unpack t)
{-# INLINEABLE string #-}

-- | Parses a space character that is not a newline.
-- >>> runParser spaceNoNewline " a"
-- Result ("a",' ')
spaceNoNewline :: (Input i, Token i ~ Char) => Parser i e Char
spaceNoNewline = satisfy $ \c -> isSpace c && c /= '\n'
{-# INLINEABLE spaceNoNewline #-}

-- | Parses a sequence of space characters that are not newlines.
-- >>> runParser spacesNoNewline " a"
-- Result ("a"," ")
spacesNoNewline :: (Input i, Token i ~ Char) => Parser i e i
spacesNoNewline = fromString <$> many spaceNoNewline
{-# INLINEABLE spacesNoNewline #-}

-- | Parses a space character.
-- >>> runParser space " a"
-- Result ("a",' ')
space :: (Input i, Token i ~ Char) => Parser i e Char
space = satisfy isSpace
{-# INLINEABLE space #-}

-- | Parses a sequence of space characters.
-- >>> runParser spaces " a"
-- Result ("a"," ")
spaces :: (Input i, Token i ~ Char) => Parser i e i
spaces = fromString <$> many space
{-# INLINEABLE spaces #-}

-- | Parses a letter.
-- >>> runParser letter "abc"
-- Result ("bc",'a')
letter :: (Input i, Token i ~ Char) => Parser i e Char
letter = satisfy isLetter
{-# INLINEABLE letter #-}

-- | Parses a sequence of letters.
-- >>> runParser letters "abc"
-- Result ("","abc")
letters :: (Input i, Token i ~ Char) => Parser i e i
letters = fromString <$> some letter
{-# INLINEABLE letters #-}

-- | Parses a symbol.
-- >>> runParser (symbol "abc") "abcdef"
-- Result ("def","abc")
symbol :: (Input i, Token i ~ Char) => i -> Parser i e i
symbol = ignoreSpaces . string
{-# INLINEABLE symbol #-}

-- | Parses any symbol from a list of symbols.
-- >>> runParser (anySymbol ["abc", "def"]) "defghi"
-- Result ("ghi","def")
anySymbol :: (Input i, Token i ~ Char) => [i] -> Parser i e i
anySymbol = foldr1 (<|>) . fmap symbol
{-# INLINEABLE anySymbol #-}

-- | Parses a value between two parsers.
-- >>> runParser (between (char '[') (char ']') (char 'a')) "[a]"
-- Result ("", 'a')
between :: Parser i e o -> Parser i e o' -> Parser i e o'' -> Parser i e o''
between open close p = open *> p <* close
{-# INLINEABLE between #-}

-- | Parses a value between two symbols.
-- >>> runParser (betweenSymbols "[" "]" (char 'a')) "[a]"
-- Result ("", 'a')
betweenSymbols :: (Input i, Token i ~ Char) => i -> i -> Parser i e a -> Parser i e a
betweenSymbols open close = between (symbol open) (symbol close)
{-# INLINEABLE betweenSymbols #-}

-- | Parses a value between brackets.
-- >>> runParser (brackets (char 'a')) "[a]"
-- Result ("", 'a')
brackets :: (Input i, Token i ~ Char) => Parser i e a -> Parser i e a
brackets = betweenSymbols "[" "]"
{-# INLINEABLE brackets #-}

-- | Parses a value between braces.
-- >>> runParser (braces (char 'a')) "{a}"
-- Result ("", 'a')
braces :: (Input i, Token i ~ Char) => Parser i e a -> Parser i e a
braces = betweenSymbols "{" "}"
{-# INLINEABLE braces #-}

-- | Parses a value between parentheses.
-- >>> runParser (parens (char 'a')) "(a)"
-- Result ("", 'a')
parens :: (Input i, Token i ~ Char) => Parser i e a -> Parser i e a
parens = betweenSymbols "(" ")"
{-# INLINEABLE parens #-}

-- | Parses a value between spaces.
-- >>> runParser (ignoreSpaces (char 'a')) " a"
-- Result ("", 'a')
ignoreSpaces :: (Input i, Token i ~ Char) => Parser i e a -> Parser i e a
ignoreSpaces = between spaces spaces
{-# INLINEABLE ignoreSpaces #-}

-- | Parses the end of the input.
-- >>> runParser eof ""
-- Result ("",())
eof :: (Input i) => Parser i e ()
eof = Parser $ \case
  input | Parser.Input.null input -> Result (input, ())
  _ -> Errors [EndOfInput]
{-# INLINEABLE eof #-}

-- | Parses the next token without consuming it.
-- >>> runParser lookahead "a"
-- Result ("a",Just 'a')
lookahead :: (Input i, Token i ~ Char) => Parser i i (Maybe Char)
lookahead = Parser $ \case
  input
    | Parser.Input.null input -> Errors [EndOfInput]
    | otherwise -> Result (input, Parser.Input.head input)
{-# INLINEABLE lookahead #-}

-- | Parses a left-associative chain of values.
-- >>> runParser (chainl1 (char 'a') (char '+')) "a+a+a"
-- Result ("","+")
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

-- | Parses a list of values until the end parser succeeds.
-- >>> runParser (manyTill (char 'a') (char 'b')) "aaab"
-- Result ("b","aaa")
manyTill :: Parser i e a -> Parser i e b -> Parser i e [a]
manyTill p end = go
  where
    go = ([] <$ end) <|> ((:) <$> p <*> go)

-- | Tries to parse a value.
-- If the parser fails, it returns an empty result.
-- >>> runParser (try (char 'a')) "b"
-- Errors []
try :: Parser i e a -> Parser i e a
try p = Parser $ \input -> case runParser p input of
  Errors _ -> Errors []
  res -> res

-- | Parses a list of values until the end parser succeeds.
-- >>> runParser (someTill (char 'a') (char 'b')) "aaab"
-- Result ("b","aaa")
someTill :: Parser i e a -> Parser i e b -> Parser i e [a]
someTill p end = (:) <$> p <*> manyTill p end
{-# INLINEABLE someTill #-}

-- | Parses a newline character.
-- >>> runParser newline "\na"
-- Result ("a",'\n')
newline :: (Input i, Token i ~ Char) => Parser i e Char
newline = char '\n'
{-# INLINEABLE newline #-}
