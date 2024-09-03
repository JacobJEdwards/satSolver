{-# LANGUAGE OverloadedStrings #-}

module Sudoku.Parser (parseSudoku, parseSudokuFile, parse) where

import Data.Text (Text)
import qualified Data.Text as Text
import Parser.Parsec
import Sudoku.Solver (Sudoku(..))

parseLiteral :: Parser Text Text Int
parseLiteral = read . (:[]) <$> digit <* many space

parseLine :: Parser Text Text [Int]
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

parseSudokuFile :: Text -> IO (Maybe Sudoku)
parseSudokuFile filename = do
  contents <- readFile (Text.unpack filename)
  return $ parse (Text.pack contents)
