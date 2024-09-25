{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module SAT.DIMACS.CNF (toExpr, fromExpr, type Clause, type CNF (..), exampleCNF, type Literal) where

import Data.Kind (type Type)
import Data.Text (type Text)
import SAT.Expr (type Expr (And, Not, Or, Var))

type Literal :: Type
type Literal = Int

type Clause :: Type
type Clause = [Literal]

type CNF :: Type
data CNF = CNF
  { numVars :: Integer,
    numClauses :: Integer,
    clauses :: [Clause],
    comments :: [Text]
  }
  deriving stock (Eq, Show)

toExpr :: [Clause] -> Expr Literal
toExpr = foldr1 And . map toOr
  where
    toOr :: Clause -> Expr Literal
    toOr = foldr1 Or . map toLiteral

    toLiteral :: Literal -> Expr Literal
    toLiteral n
      | n < 0 = Not $ Var $ abs n
      | otherwise = Var n

fromExpr :: Expr Literal -> CNF
fromExpr expr =
  CNF
    { numVars = -1, -- fix
      numClauses = fromIntegral $ length clauseList,
      clauses = clauseList,
      comments = ["This is a CNF formula generated from an expression."]
    }
  where
    clauseList :: [Clause]
    clauseList = fromAnd expr

    fromAnd :: Expr Literal -> [Clause]
    fromAnd (And e1 e2) = fromAnd e1 <> fromAnd e2
    fromAnd e = [fromOr e]

    fromOr :: Expr Literal -> Clause
    fromOr (Or e1 e2) = fromOr e1 <> fromOr e2
    fromOr e = [fromLiteral e]

    fromLiteral :: Expr Literal -> Literal
    fromLiteral (Not (Var n)) = negate n
    fromLiteral (Var n) = n
    fromLiteral _ = error "Invalid literal"

exampleCNF :: CNF
exampleCNF = CNF 3 2 [[1, 2], [-1, 3]] ["This is an example CNF formula."]
