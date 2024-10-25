{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SAT.DIMACS.Parser (parseCNF, parseFile, parse) where

import Data.Char (isDigit)
import Data.Text (type Text)
import Data.Text qualified as Text
import Parser (char, digit, many, optional, runParser, satisfy, some, spaces, symbol, type Parser, type Result (Result))
import SAT.DIMACS.CNF (type DIMACS (DIMACS, clauses, comments, numClauses, numVars), type Clause, type Literal)

parseComment :: Parser Text Text Text
parseComment = symbol "c" *> (Text.pack <$> many (satisfy (/= '\n'))) <* char '\n'

parseComments :: Parser Text Text [Text]
parseComments = many parseComment

parseHeader :: Parser Text Text (Integer, Integer)
parseHeader = do
  _ <- symbol "p"
  _ <- symbol "cnf"
  _ <- spaces
  vars <- read <$> some digit
  _ <- spaces
  clauses' <- read <$> some digit
  _ <- char '\n'
  return (vars, clauses')

parseLiteral :: Parser Text Text Literal
parseLiteral = do
  _ <- spaces
  negation <- optional $ char '-'
  n <- read <$> some (satisfy (\c -> isDigit c && c /= '0'))
  let n' = if negation == Just '-' then negate n else n
  _ <- spaces
  return n'

parseClause :: Parser Text Text Clause
parseClause = do
  parsedClause <- some (parseLiteral <* spaces)
  _ <- char '0'
  return parsedClause

parseClauses :: Parser Text Text [Clause]
parseClauses = many parseClause

parseCNF' :: Parser Text Text DIMACS
parseCNF' = do
  comments' <- parseComments
  (vars, clauses') <- parseHeader
  parsedClauses <- parseClauses
  return $
    DIMACS
      { numVars = vars,
        numClauses = clauses',
        clauses = parsedClauses,
        comments = comments'
      }

parseCNF :: Text -> Result Text Text (Text, DIMACS)
parseCNF = runParser parseCNF'

parse :: Text -> Maybe DIMACS
parse input = case parseCNF input of
  Result (_, cnf) -> Just cnf
  _ -> Nothing

parseFile :: Text -> IO (Maybe DIMACS)
parseFile filename = do
  contents <- readFile (Text.unpack filename)
  case runParser parseCNF' (Text.pack contents) of
    Result (_, cnf) -> return $ Just cnf
    _ -> return Nothing
