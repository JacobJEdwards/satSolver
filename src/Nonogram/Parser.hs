module Nonogram.Parser (parse) where
  
import Data.Text (Text)
import Parser.Parsec
import Nonogram.Solver (Nonogram(..))

parseSudoku :: Parser Text Text Nonogram
parseSudoku = undefined

parse :: Text -> Maybe Nonogram
parse input = case runParser parseSudoku input of
  Result (_, sudoku) -> Just sudoku
  Errors _ -> Nothing
  
