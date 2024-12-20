{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE Strict #-}

-- |
-- Module      : Nonogram.Parser
-- Description : Exports the Nonogram parser module.
module Nonogram.Parser (parse) where

import Data.Text (type Text)
import Nonogram.Solver (Nonogram)
import Parser.Parsec (runParser, type Parser, type Result (type Errors, type Result))

-- | Parses a nonogram from a string.
parseNonogram :: Parser Text Text Nonogram
parseNonogram = undefined

-- | Parses a nonogram from a string.
-- Returns 'Nothing' if the input is invalid.
-- Returns 'Just' the nonogram otherwise.
--
-- >>> todo
parse :: Text -> Maybe Nonogram
parse input = case runParser parseNonogram input of
  Result (_, nonogram) -> Just nonogram
  Errors _ -> Nothing
