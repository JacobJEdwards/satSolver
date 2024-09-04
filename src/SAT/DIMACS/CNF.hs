{-# LANGUAGE OverloadedStrings #-}

module SAT.DIMACS.CNF (toExpr, fromExpr, Clause, CNF(..), exampleCNF) where

import SAT.Expr

import Data.Text (Text)

type Clause = [Int]

data CNF = CNF
  { numVars :: Int,
    numClauses :: Int,
    clauses :: [Clause],
    comments :: [Text]
  }
  deriving (Eq, Show)
  
toExpr :: [Clause] -> Expr Int
toExpr = foldr1 And . map (foldr1 Or . map toLiteral)
  where
    toLiteral :: Int -> Expr Int
    toLiteral n
      | n < 0 = Not $ Var $ toEnum $ abs n
      | otherwise = Var $ toEnum n

fromExpr :: Expr Int -> CNF
fromExpr expr = CNF {
    numVars = -1, -- fix
    numClauses = length ands,
    clauses = ands,
    comments = ["This is a CNF formula generated from an expression."]
}
  where
    ands :: [Clause]
    ands = fromAnd expr
    
    fromLiteral :: Expr Int -> Int
    fromLiteral (Not (Var n)) = -fromEnum n
    fromLiteral (Var n) = fromEnum n
    fromLiteral _ = error "Invalid literal"

    fromAnd :: Expr Int -> [Clause]
    fromAnd (And e1 e2) = fromAnd e1 ++ fromAnd e2
    fromAnd e = [fromOr e]

    fromOr :: Expr Int -> Clause
    fromOr (Or e1 e2) = fromOr e1 ++ fromOr e2
    fromOr e = [fromLiteral e]

exampleCNF :: CNF
exampleCNF = CNF 3 2 [[1, 2], [-1, 3]] ["This is an example CNF formula."]