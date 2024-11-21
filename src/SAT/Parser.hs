{-|
Module      : SAT.Parser
Description : Exports the SAT parser module.
-}

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module SAT.Parser (parse) where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Text (type Text)
import Parser (anySymbol, chainl1, digit, parens, runParser, some, type Parser, type Result (Errors, Result))
import SAT.Expr (type Expr (And, Not, Or, Val, Var, Implies))

-- | Parses an expression from a string.
-- Returns 'Nothing' if the input is invalid.
-- Returns 'Just' the expression otherwise.
-- 
-- >>> parse "1 and 2 or 3"
-- Just (1 `And` (2 `Or` 3))
parse :: Text -> Maybe (Expr Int)
parse input = case parseExpr input of
  Result (_, expr') -> Just expr'
  Errors _ -> Nothing

-- | Parses an expression from a string.
parseExpr :: Text -> Result Text Text (Text, Expr Int)
parseExpr = runParser expr

-- | Parses an expression.
expr :: Parser Text Text (Expr Int)
expr = chainl1 term orOp <|> chainl1 term impliesOp

-- | Parses a term.
term :: Parser Text Text (Expr Int)
term = chainl1 factor andOp

-- | Parses a factor.
factor :: Parser Text Text (Expr Int)
factor = parens expr <|> notOp <|> literal <|> var

-- | Parses a variable.
var :: Parser Text Text (Expr Int)
var = Var <$> (read <$> some digit)

-- | Parses a not operation.
notOp :: Parser Text Text (Expr Int)
notOp = Not <$> (anySymbol ["¬", "not"] *> factor)

-- | Parses an and operation.
andOp :: Parser Text Text (Expr a -> Expr a -> Expr a)
andOp = anySymbol ["∧", "and"] $> And

-- | Parses an or operation.
orOp :: Parser Text Text (Expr a -> Expr a -> Expr a)
orOp = anySymbol ["∨", "or"] $> Or

-- | Parses an implies operation.
impliesOp :: Parser Text Text (Expr a -> Expr a -> Expr a)
impliesOp = anySymbol ["=>", "implies"] $> Implies

-- | Parses a literal.
literal :: Parser Text Text (Expr a)
literal = Val <$> (true <|> false)
  where
    -- | Parses a true literal.
    true :: Parser Text Text Bool
    true = anySymbol ["true"] $> True

    -- | Parses a false literal.
    false :: Parser Text Text Bool
    false = anySymbol ["false"] $> False
