{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module SAT.DIMACS.CNF (toExpr, fromExpr, type Clause, type DIMACS (..), exampleDIMACS, type Literal, toCNF, fromCNF) where

import Data.Kind (type Type)
import Data.Text (type Text)
import SAT.Expr (type Expr (And, Not, Or, Var, Implies))
import SAT.CNF (type CNF(CNF))
import SAT.CNF qualified as CNF
import Data.Set qualified as Set

type Literal :: Type
type Literal = Int

type Clause :: Type
type Clause = [Literal]

type DIMACS :: Type
data DIMACS = DIMACS
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

fromExpr :: Expr Literal -> DIMACS
fromExpr expr =
  DIMACS
    { numVars = numVars',
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
    fromOr (Implies e1 e2) = fromOr (Or (Not e1) e2)
    fromOr e = [fromLiteral e]

    fromLiteral :: Expr Literal -> Literal
    fromLiteral (Not (Var n)) = negate n
    fromLiteral (Var n) = n
    fromLiteral _ = error "Invalid literal"
    
    numVars' :: Integer
    numVars' = fromIntegral . Set.size . Set.fromList . map abs $ concat clauseList
    
toCNF :: DIMACS -> CNF
toCNF = CNF . toCNF' . clauses
  where
    toCNF' :: [Clause] -> [CNF.Clause]
    toCNF' = map toClause
    
    toClause :: Clause -> CNF.Clause
    toClause = map toLiteral
    
    toLiteral :: Literal -> CNF.Literal
    toLiteral = id

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
   


exampleDIMACS :: DIMACS
exampleDIMACS = DIMACS 3 2 [[1, 2], [-1, 3]] ["This is an example CNF formula."]
