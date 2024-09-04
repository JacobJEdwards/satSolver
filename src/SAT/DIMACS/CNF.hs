{-# LANGUAGE OverloadedStrings #-}

module SAT.DIMACS.CNF (toExpr, fromExpr, Clause, CNF(..), exampleCNF, Literal) where

import SAT

import Data.Text (Text)

type Literal = Int

type Clause = [Literal]

data CNF = CNF
  { numVars :: Integer,
    numClauses :: Integer,
    clauses :: [Clause],
    comments :: [Text]
  }
  deriving (Eq, Show)
  
toExpr :: [Clause] -> Expr Literal
toExpr = foldr1 And . map (foldr1 Or . map toLiteral)
  where
    toLiteral :: Literal -> Expr Literal
    toLiteral n
      | n < 0 = Not $ Var $ abs n
      | otherwise = Var n

fromExpr :: Expr Literal -> CNF
fromExpr expr = CNF {
    numVars = -1, -- fix
    numClauses = fromIntegral $ length clauseList,
    clauses = clauseList,
    comments = ["This is a CNF formula generated from an expression."]
}
  where
    clauseList :: [Clause]
    clauseList = fromAnd expr
    
    fromLiteral :: Expr Literal -> Literal
    fromLiteral (Not (Var n)) = negate n
    fromLiteral (Var n) = n
    fromLiteral _ = error "Invalid literal"

    fromAnd :: Expr Literal -> [Clause]
    fromAnd (And e1 e2) = fromAnd e1 <> fromAnd e2
    fromAnd e = [fromOr e]

    fromOr :: Expr Literal -> Clause
    fromOr (Or e1 e2) = fromOr e1 <> fromOr e2
    fromOr e = [fromLiteral e]

exampleCNF :: CNF
exampleCNF = CNF 3 2 [[1, 2], [-1, 3]] ["This is an example CNF formula."]