{-# LANGUAGE ExplicitNamespaces #-}

module Nonogram.Parser (parse) where

import Data.Text (type Text)
import Nonogram.Solver (Nonogram)
import Parser.Parsec (runParser, type Parser, type Result (type Errors, type Result))

parseNonogram :: Parser Text Text Nonogram
parseNonogram = undefined

parse :: Text -> Maybe Nonogram
parse input = case runParser parseNonogram input of
  Result (_, nonogram) -> Just nonogram
  Errors _ -> Nothing
