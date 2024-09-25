{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module Sudoku.Parser (parse) where

import Data.Text (type Text)
import Parser.Parsec (digit, many, runParser, some, space, symbol, type Parser, type Result (Errors, Result))
import SAT.DIMACS qualified as DIMACS
import Sudoku.Solver (type Size, type Sudoku (Sudoku))

parseLiteral :: Parser Text Text DIMACS.Literal
parseLiteral = read . (: []) <$> digit <* many space

parseLine :: Parser Text Text DIMACS.Clause
parseLine = some parseLiteral <* symbol "-"

parseSudoku :: Parser Text Text Sudoku
parseSudoku = do
  rows <- some parseLine
  return $ Sudoku rows $ size' rows
  where
    size' :: [[a]] -> Size
    size' = toEnum . length

parse :: Text -> Maybe Sudoku
parse input = case runParser parseSudoku input of
  Result (_, sudoku) -> Just sudoku
  Errors _ -> Nothing
