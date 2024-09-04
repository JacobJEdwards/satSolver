{-# LANGUAGE OverloadedStrings #-}

module Sudoku.Parser (parse) where

import Data.Text (Text)
import Parser.Parsec
import Sudoku.Solver (Sudoku(..))
import qualified SAT.DIMACS as DIMACS

parseLiteral :: Parser Text Text DIMACS.Literal
parseLiteral = read . (:[]) <$> digit <* many space

parseLine :: Parser Text Text DIMACS.Clause
parseLine = some parseLiteral <* symbol "-"

parseSudoku :: Parser Text Text Sudoku
parseSudoku = do
  rows <- some parseLine
  return $ Sudoku rows $ size' rows
  where size' = toEnum . length

parse :: Text -> Maybe Sudoku
parse input = case runParser parseSudoku input of
  Result (_, sudoku) -> Just sudoku
  Errors _ -> Nothing
  
