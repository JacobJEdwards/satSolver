{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.DIMACS (CNF(..), parseCNF, toExpr, Clause, parseDIMACSFile) where

import Expr
import Parser.Parsec

import Control.Applicative (many, some, optional)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (isDigit)

type Clause = [Int]

data CNF = CNF {
  numVars :: Int,
  numClauses :: Int,
  clauses :: [Clause],
  comments :: [Text]
} deriving (Eq, Show)


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

toExpr :: [Clause] -> Expr Int
toExpr = foldr1 And . map (foldr1 Or . map toLiteral)
  where
    toLiteral ::  Int -> Expr Int
    toLiteral n
      | n < 0 = Not $ Var $ toEnum $ abs n
      | otherwise = Var $ toEnum n

parseCNF' :: Parser Text Text CNF
parseCNF' = do
  comments' <- parseComments
  (vars, clauses') <- parseHeader
  parsedClauses <- parseClauses
  return $ CNF vars clauses' parsedClauses comments'
  
parseCNF :: Text -> Result Text Text (Text, CNF)
parseCNF = runParser parseCNF'

parseDIMACSFile :: Text -> IO CNF
parseDIMACSFile filename = do
  contents <- readFile (Text.unpack filename)
  case runParser parseCNF' (Text.pack contents) of
    Result (_, cnf) -> return cnf
    Errors errs -> error $ "Errors: " ++ show errs