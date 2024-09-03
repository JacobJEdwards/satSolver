{-# LANGUAGE OverloadedStrings #-}

module SAT.Parser (parse) where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Text (Text)
import Parser.Parsec
import SAT.Expr

parse :: Text -> Result Text Text (Text, Expr Char)
parse = runParser expr

expr :: Parser Text Text (Expr Char)
expr = chainl1 term orOp

term :: Parser Text Text (Expr Char)
term = chainl1 factor andOp

factor :: Parser Text Text (Expr Char)
factor = parens expr <|> notOp <|> literal <|> var

var :: Parser Text Text (Expr Char)
var = Var <$> letter

notOp :: Parser Text Text (Expr Char)
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
