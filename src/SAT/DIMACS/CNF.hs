{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : SAT.DIMACS.CNF
-- Description : Exports the CNF module.
module SAT.DIMACS.CNF (toExpr, fromExpr, type Clause, type DIMACS (..), exampleDIMACS, type Literal, toCNF, fromCNF, invert) where

import Control.Parallel.Strategies (NFData)
import Data.Kind (type Type)
import Data.Set qualified as Set
import Data.Text (type Text)
import GHC.Generics (Generic)
import SAT.CNF (varOfLiteral, type CNF (CNF))
import SAT.CNF qualified as CNF
import SAT.Expr (type Expr (And, Implies, Not, Or, Var))

-- | The literal type.
type Literal :: Type
type Literal = Int

-- | The clause type.
type Clause :: Type
type Clause = [Literal]

-- | The DIMACS type.
type DIMACS :: Type
data DIMACS = DIMACS
  { numVars :: !Integer,
    numClauses :: !Integer,
    clauses :: ![Clause],
    comments :: [Text]
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance NFData DIMACS

-- | Converts a list of clauses to an expression.
toExpr :: [Clause] -> Expr Literal
toExpr = foldr1 And . fmap toOr
  where
    toOr :: Clause -> Expr Literal
    toOr = foldr1 Or . fmap toLiteral

    toLiteral :: Literal -> Expr Literal
    toLiteral n
      | n < 0 = Not $ Var $ varOfLiteral n
      | otherwise = Var n

-- | Converts an expression to dimacs.
--
-- >>> fromExpr (Or (Var 1) (Var 2))
-- DIMACS {numVars = 2, numClauses = 1, clauses = [[1,2]], comments = ["This is a CNF formula generated from an expression."]}
fromExpr :: Expr Literal -> DIMACS
fromExpr expr =
  DIMACS
    { numVars = numVars',
      numClauses = fromIntegral $ length clauseList,
      clauses = clauseList,
      comments = ["This is a CNF formula generated from an expression."]
    }
  where
    -- \| The list of clauses.
    clauseList :: [Clause]
    clauseList = fromAnd expr

    -- \| Converts an expression to a list of clauses.
    fromAnd :: Expr Literal -> [Clause]
    fromAnd (And e1 e2) = fromAnd e1 <> fromAnd e2
    fromAnd e = pure $ fromOr e

    -- \| Converts an expression to a clause.
    fromOr :: Expr Literal -> Clause
    fromOr (Or e1 e2) = fromOr e1 <> fromOr e2
    fromOr (Implies e1 e2) = fromOr $ Or (Not e1) e2
    fromOr e = pure $ fromLiteral e

    -- \| Converts an expression to a literal.
    fromLiteral :: Expr Literal -> Literal
    fromLiteral (Not (Var n)) = negate n
    fromLiteral (Var n) = n
    fromLiteral _ = error "Invalid literal"

    -- \| The number of variables.
    numVars' :: Integer
    numVars' = fromIntegral . Set.size . Set.fromList . fmap abs $ concat clauseList

invert :: Literal -> Literal
invert = negate

-- | Converts a DIMACS formula to CNF.
--
-- >>> toCNF exampleDIMACS
-- CNF {clauses = [[1,2],[-1,3]]}
toCNF :: DIMACS -> CNF
toCNF = CNF . toCNF' . clauses
  where
    toCNF' :: [Clause] -> [CNF.Clause]
    toCNF' = fmap toClause

    toClause :: Clause -> CNF.Clause
    toClause c = CNF.Clause {CNF.literals = fmap toLiteral c, CNF.watched = (0, 0)}

    toLiteral :: Literal -> CNF.Literal
    toLiteral = id

-- | Converts a CNF formula to DIMACS.
--
-- >>> fromCNF (CNF [[1,2],[-1,3]])
-- DIMACS {numVars = 3, numClauses = 2, clauses = [[1,2],[-1,3]], comments = ["This is a CNF formula generated from a CNF formula."]}
fromCNF :: CNF -> DIMACS
fromCNF (CNF clauses') =
  DIMACS
    { numVars = numVars',
      numClauses = fromIntegral $ length clauses',
      clauses = clauseList,
      comments = ["This is a CNF formula generated from a CNF formula."]
    }
  where
    clauseList :: [Clause]
    clauseList = fmap fromClause clauses'

    fromClause :: CNF.Clause -> Clause
    fromClause (CNF.Clause {CNF.literals}) = fmap fromLiteral literals

    fromLiteral :: CNF.Literal -> Literal
    fromLiteral = id

    numVars' :: Integer
    numVars' = fromIntegral . Set.size . Set.fromList . fmap abs $ concat clauseList

-- | An example DIMACS formula.
exampleDIMACS :: DIMACS
exampleDIMACS = DIMACS 3 2 [[1, 2], [-1, 3]] ["This is an example CNF formula."]
