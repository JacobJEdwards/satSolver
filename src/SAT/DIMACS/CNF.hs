{-|
Module      : SAT.DIMACS.CNF
Description : Exports the CNF module.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module SAT.DIMACS.CNF (toExpr, fromExpr, type Clause, type DIMACS (..), exampleDIMACS, type Literal, toCNF, fromCNF) where

import Data.Kind (type Type)
import Data.Text (type Text)
import SAT.Expr (type Expr (And, Not, Or, Var, Implies))
import SAT.CNF (type CNF(CNF))
import SAT.CNF qualified as CNF
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Control.Parallel.Strategies (NFData)

-- | The literal type.
type Literal :: Type
type Literal = Int

-- | The clause type.
type Clause :: Type
type Clause = [Literal]

-- | The DIMACS type.
type DIMACS :: Type
data DIMACS = DIMACS
  { numVars :: Integer,
    numClauses :: Integer,
    clauses :: [Clause],
    comments :: [Text]
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance NFData DIMACS

-- | Converts a list of clauses to an expression.
toExpr :: [Clause] -> Expr Literal
toExpr = foldr1 And . map toOr
  where
    toOr :: Clause -> Expr Literal
    toOr = foldr1 Or . map toLiteral

    toLiteral :: Literal -> Expr Literal
    toLiteral n
      | n < 0 = Not $ Var $ abs n
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
    -- | The list of clauses.
    clauseList :: [Clause]
    clauseList = fromAnd expr

    -- | Converts an expression to a list of clauses.
    fromAnd :: Expr Literal -> [Clause]
    fromAnd (And e1 e2) = fromAnd e1 <> fromAnd e2
    fromAnd e = [fromOr e]

    -- | Converts an expression to a clause.
    fromOr :: Expr Literal -> Clause
    fromOr (Or e1 e2) = fromOr e1 <> fromOr e2
    fromOr (Implies e1 e2) = fromOr (Or (Not e1) e2)
    fromOr e = [fromLiteral e]

    -- | Converts an expression to a literal.
    fromLiteral :: Expr Literal -> Literal
    fromLiteral (Not (Var n)) = negate n
    fromLiteral (Var n) = n
    fromLiteral _ = error "Invalid literal"
    
    -- | The number of variables.
    numVars' :: Integer
    numVars' = fromIntegral . Set.size . Set.fromList . map abs $ concat clauseList
    
-- | Converts a DIMACS formula to CNF.
-- 
-- >>> toCNF exampleDIMACS
-- CNF {clauses = [[1,2],[-1,3]]}
toCNF :: DIMACS -> CNF
toCNF = CNF . toCNF' . clauses
  where
    toCNF' :: [Clause] -> [CNF.Clause]
    toCNF' = map toClause
    
    toClause :: Clause -> CNF.Clause
    toClause = map toLiteral
    
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
    clauseList = map fromClause clauses'

    fromClause :: CNF.Clause -> Clause
    fromClause = map fromLiteral

    fromLiteral :: CNF.Literal -> Literal
    fromLiteral = id

    numVars' :: Integer
    numVars' = fromIntegral . Set.size . Set.fromList . map abs $ concat clauseList
   

-- | An example DIMACS formula.
exampleDIMACS :: DIMACS
exampleDIMACS = DIMACS 3 2 [[1, 2], [-1, 3]] ["This is an example CNF formula."]
