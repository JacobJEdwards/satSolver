{-|
Module      : SAT.CNF
Description : Exports the CNF module.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module SAT.CNF (applyLaws, toCNF, type CNF(CNF), type Clause, type Literal, addClause, type Assignment, type DecisionLevel, isNegative, tseitin, toTseitin) where

import SAT.Expr (type Expr(Not, And, Or, Val, Var, Implies))
import Data.IntMap (type IntMap)
import GHC.Generics (Generic)
import Control.Parallel.Strategies (NFData)
import Control.Monad.State.Strict (State, get, put, runState)

type DecisionLevel = Int
type Assignment = IntMap (Bool, DecisionLevel)

-- data cnf is list of clauses

-- consider moving these into vector or seq
-- | The CNF type.
newtype CNF = CNF [Clause]
  deriving stock (Eq, Show, Ord, Generic)

deriving anyclass instance NFData CNF

-- data CNF where 
--   CNF :: (Traversable t, Semigroup (t Clause), Applicative t) => t Clause -> CNF

-- | The clause type.
type Clause = [Literal]

-- | The literal type.
type Literal = Int

-- https://en.wikipedia.org/wiki/Tseytin_transformation -> look into this

-- https://hackage.haskell.org/package/picologic-0.3.0/docs/src/Picologic-Tseitin.html
-- | Applies the laws of De Morgan to an expression.
-- 
-- >>> deMorgansLaws (Not (Not (Var 1)))
-- Var 1
deMorgansLaws :: Expr a -> Expr a
deMorgansLaws (Not (Not e)) = deMorgansLaws e
deMorgansLaws (Not (And e1 e2)) = Or (deMorgansLaws $ Not e1) (deMorgansLaws $ Not e2)
deMorgansLaws (Not (Or e1 e2)) = And (deMorgansLaws $ Not e1) (deMorgansLaws $ Not e2)
deMorgansLaws (Not (Val b)) = Val $ not b
deMorgansLaws (And e1 e2) = And (deMorgansLaws e1) (deMorgansLaws e2)
deMorgansLaws (Or e1 e2) = Or (deMorgansLaws e1) (deMorgansLaws e2)
deMorgansLaws (Not e) = Not $ deMorgansLaws e
deMorgansLaws (Implies e1 e2) = Or (deMorgansLaws $ Not e1) (deMorgansLaws e2)
deMorgansLaws e = e

-- | Applies the laws of distribution to an expression.
-- 
-- >>> distributiveLaws (Or (And (Var 1) (Var 2)) (Var 3))
-- And (Or (Var 3) (Var 1)) (Or (Var 3) (Var 2))
distributiveLaws :: Expr a -> Expr a
distributiveLaws (Or e1 (And e2 e3)) = And (Or (distributiveLaws e1) (distributiveLaws e2)) (Or (distributiveLaws e1) (distributiveLaws e3))
distributiveLaws (Or (And e1 e2) e3) = And (Or (distributiveLaws e3) (distributiveLaws e1)) (Or (distributiveLaws e3) (distributiveLaws e2))
distributiveLaws (Or e1 e2) = Or (distributiveLaws e1) (distributiveLaws e2)
distributiveLaws (And e1 e2) = And (distributiveLaws e1) (distributiveLaws e2)
distributiveLaws (Not e) = Not $ distributiveLaws e
distributiveLaws (Implies e1 e2) = Implies (distributiveLaws e1) (distributiveLaws e2)
distributiveLaws e = e

-- | Applies the laws of distribution and De Morgan to an expression.
-- 
-- >>> applyLaws (Or (And (Var 1) (Var 2)) (Not (Var 3)))
-- And (Or (Not (Var 3)) (Var 1)) (Or (Not (Var 3)) (Var 2))
applyLaws :: (Eq a) => Expr a -> Expr a
applyLaws expr
  | expr == expr' = expr
  | otherwise = applyLaws expr'
  where
    expr' = distributiveLaws $ deMorgansLaws expr
{-# INLINEABLE applyLaws #-}


-- | Converts an expression to CNF.
-- 
-- >>> toCNF (Or (And (Var 1) (Var 2)) (Not (Var 3)))
-- CNF [[-3,1],[-3,2]]
toCNF :: Expr Int -> CNF
toCNF expr = CNF $ toClauses cnf
  where
    cnf :: Expr Int
    cnf = applyLaws expr

    toClauses :: (Traversable t, Semigroup (t Clause), Applicative t) => Expr Int -> t Clause
    toClauses (And e1 e2) = toClauses e1 <> toClauses e2
    toClauses e = pure $ toClause e

    toClause :: Expr Int -> Clause
    toClause (Or e1 e2) = toClause e1 <> toClause e2
    toClause e = pure $ toLiteral e

    toLiteral :: Expr Int -> Literal
    toLiteral (Not (Var n)) = negate n
    toLiteral (Var n) = n
    toLiteral l = error $ "Invalid literal" ++ show l
{-# INLINEABLE toCNF #-}

toTseitin :: Expr Int -> CNF
toTseitin expr = CNF clauses
  where 
    tseitin' :: State Int (CNF, Literal)
    tseitin' = tseitin expr

    tseitin'' :: State Int CNF
    tseitin'' = do
      (CNF clauses', l) <- tseitin'
      return $ CNF $ [[l]] <> clauses'

    (CNF clauses, _) = runState tseitin'' (highestVar expr + 1)
{-# INLINEABLE toTseitin #-}

-- | Adds a clause to a CNF.
-- 
-- >>> addClause (CNF [[1,2]]) [3]
-- CNF [[3],[1,2]]
addClause :: CNF -> Clause -> CNF
addClause (CNF clauses) clause = CNF $ pure clause <> clauses
{-# INLINEABLE addClause #-}

-- | Checks if a literal is negative.
-- 
-- >>> isNegative (-1)
-- True
-- 
-- >>> isNegative 1
-- False
-- 
-- prop> isNegative x == (x < 0)
isNegative :: Literal -> Bool
isNegative = (< 0)
{-# INLINEABLE isNegative #-}

type FreshVariable = State Int

freshVariable :: FreshVariable Literal
freshVariable = do
  n <- get
  put $ n + 1
  return n

-- | Returns the highest variable in an expression.
-- In order to know where to start the fresh variables.
highestVar :: Expr Int -> Int
highestVar (Var n) = n
highestVar (Val _) = 0
highestVar (Not e) = highestVar e
highestVar (And e1 e2) = max (highestVar e1) (highestVar e2)
highestVar (Or e1 e2) = max (highestVar e1) (highestVar e2)
highestVar (Implies e1 e2) = max (highestVar e1) (highestVar e2)

tseitin :: Expr Int -> State Int (CNF, Literal)
tseitin (Var n) = return (CNF [], n)
tseitin (Val b) = return (CNF [], if b then 1 else -1)
tseitin (Not e) = do
  (CNF clauses, l) <- tseitin e
  l' <- freshVariable
  let clauses' = [[-l', -l], [l', l]]
  return (CNF (clauses <> clauses'), l')
tseitin (And e1 e2) = do
  (CNF clauses1, l1) <- tseitin e1
  (CNF clauses2, l2) <- tseitin e2
  l' <- freshVariable
  let clauses = [[-l', l1], [-l', l2], [l', -l1, -l2]]
  return (CNF (clauses <> clauses1 <> clauses2), l')
tseitin (Or e1 e2) = do
  (CNF clauses1, l1) <- tseitin e1
  (CNF clauses2, l2) <- tseitin e2
  l' <- freshVariable
  let clauses = [[l', -l1], [l', -l2], [-l', l1, l2]]
  return (CNF (clauses <> clauses1 <> clauses2), l')
tseitin (Implies e1 e2) = tseitin $ Or (Not e1) e2