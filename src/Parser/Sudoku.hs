module Parser.Sudoku (parseSudoku, parseSudokuFile) where

import qualified Parser.Parsec as Parsec

import Control.Applicative (many, some)

import Data.Text (Text)
import qualified Data.Text as Text

import Sudoku (Sudoku)

parseLiteral :: Parsec.Parser Text Text Int
parseLiteral = read . show <$> Parsec.digit

parseLine :: Parsec.Parser Text Text [Int]
parseLine = some (parseLiteral <* Parsec.spaceNoNewline) <* Parsec.char '\n'

parseSudoku :: Parsec.Parser Text Text Sudoku
parseSudoku = many parseLine

parseSudokuFile :: Text -> IO Sudoku
parseSudokuFile filename = do
  contents <- readFile (Text.unpack filename)
  case Parsec.runParser parseSudoku (Text.pack contents) of
    Parsec.Result (_, sudoku) -> return sudoku
    Parsec.Errors errs -> error $ "Errors: " ++ show errs