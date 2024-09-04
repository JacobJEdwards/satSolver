{-# LANGUAGE OverloadedStrings #-}

module SAT.Parser (parse) where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Text (Text)
import Parser.Parsec
import SAT.Expr

parse :: Text -> Maybe (Expr Int)
parse input = case parseExpr input of
  Result (_, expr') -> Just expr'
  Errors _ -> Nothing

parseExpr :: Text -> Result Text Text (Text, Expr Int)
parseExpr = runParser expr

expr :: Parser Text Text (Expr Int)
expr = chainl1 term orOp

term :: Parser Text Text (Expr Int)
term = chainl1 factor andOp

factor :: Parser Text Text (Expr Int)
factor = parens expr <|> notOp <|> literal <|> var

var :: Parser Text Text (Expr Int)
var = Var <$> (read <$> some digit)

notOp :: Parser Text Text (Expr Int)
notOp = Not <$> (anySymbol ["¬", "not"] *> factor)

andOp :: Parser Text Text (Expr a -> Expr a -> Expr a)
andOp = anySymbol ["∧", "and"] $> And

orOp :: Parser Text Text (Expr a -> Expr a -> Expr a)
orOp = anySymbol ["∨", "or"] $> Or

literal :: Parser Text Text (Expr a)
literal = Const <$> (true <|> false)
  where
    true = anySymbol ["true", "1"] $> True
    false = anySymbol ["false", "0"] $> False
