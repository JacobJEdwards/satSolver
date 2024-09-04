{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SAT.DIMACS.Parser (parseCNF, parseFile, parse) where

import Parser.Parsec
import Data.Text (Text)
import qualified Data.Text as Text
import SAT.DIMACS.CNF (CNF(..), Clause)

import Data.Char (isDigit)

parseComment :: Parser Text Text Text
parseComment = symbol "c" *> (Text.pack <$> many (satisfy (/= '\n'))) <* char '\n'

parseComments :: Parser Text Text [Text]
parseComments = many parseComment

parseHeader :: Parser Text Text (Int, Int)
parseHeader = do
  _ <- symbol "p"
  _ <- symbol "cnf"
  _ <- spaces
  vars <- read <$> some digit
  _ <- spaces
  clauses' <- read <$> some digit
  _ <- char '\n'
  return (vars, clauses')

parseLiteral :: Parser Text Text Int
parseLiteral = do
  _ <- spaces
  negation <- optional $ char '-'
  n <- read <$> some (satisfy (\c -> isDigit c && c /= '0'))
  let n' = if negation == Just '-' then -n else n
  _ <- spaces
  return n'

parseClause :: Parser Text Text Clause
parseClause = do 
  parsedClause <- some (parseLiteral <* spaces)
  _ <- char '0'
  return parsedClause

parseClauses :: Parser Text Text [Clause]
parseClauses = many parseClause

parseCNF' :: Parser Text Text CNF
parseCNF' = do
  comments' <- parseComments
  (vars, clauses') <- parseHeader
  parsedClauses <- parseClauses
  return $ CNF vars clauses' parsedClauses comments'
  
parseCNF :: Text -> Result Text Text (Text, CNF)
parseCNF = runParser parseCNF'

parse :: Text -> Maybe CNF
parse input = case parseCNF input of
  Result (_, cnf) -> Just cnf
  _ -> Nothing


parseFile :: Text -> IO (Maybe CNF)
parseFile filename = do
  contents <- readFile (Text.unpack filename)
  case runParser parseCNF' (Text.pack contents) of
    Result (_, cnf) -> return $ Just cnf
    _ -> return Nothing
