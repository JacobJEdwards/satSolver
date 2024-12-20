{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

-- |
-- Module      : Sudoku.Parser
-- Description : Exports the Sudoku parser module.
module Sudoku.Parser (parse) where

import Data.Text (type Text)
import Parser.Parsec (digit, many, runParser, some, space, symbol, type Parser, type Result (Errors, Result))
import SAT.DIMACS qualified as DIMACS
import Sudoku.Solver (type Size, type Sudoku (Sudoku))

-- | Parses a literal from a string.
--
-- >>> parseLiteral "1"
-- 1
parseLiteral :: Parser Text Text DIMACS.Literal
parseLiteral = read . (: []) <$> digit <* many space

-- | Parses a line from a string.
--
-- >>> parseLine "1 2 3 -"
-- [1, 2, 3]
parseLine :: Parser Text Text DIMACS.Clause
parseLine = some parseLiteral <* symbol "-"

-- | Parses a sudoku from a string.
--
-- >>> parse "1 2 3 -\n4 5 6 -\n7 8 9 -"
parseSudoku :: Parser Text Text Sudoku
parseSudoku = do
  rows <- some parseLine
  return $ Sudoku rows $ size' rows
  where
    size' :: [[a]] -> Size
    size' = toEnum . length

-- | Parses a sudoku from a string.
-- Returns 'Nothing' if the input is invalid.
-- Returns 'Just' the sudoku otherwise.
--
-- >>> parse "1 2 3 -\n4 5 6 -\n7 8 9 -"
-- Just (Sudoku [[1, 2, 3], [4, 5, 6], [7, 8, 9]] 3)
parse :: Text -> Maybe Sudoku
parse input = case runParser parseSudoku input of
  Result (_, sudoku) -> Just sudoku
  Errors _ -> Nothing
