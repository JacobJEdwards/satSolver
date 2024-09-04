module Nonogram.Parser (parse) where
  
import Data.Text (Text)
import Parser.Parsec
import Nonogram.Solver (Nonogram(..))

parseNonogram :: Parser Text Text Nonogram
parseNonogram = undefined

parse :: Text -> Maybe Nonogram
parse input = case runParser parseNonogram input of
  Result (_, nonogram) -> Just nonogram
  Errors _ -> Nothing
  
