{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NamedFieldPuns #-}

module SAT.Preprocessing (preprocess) where

import Control.Monad.RWS.Strict (modify)
import Data.List (sortOn, nub)
import qualified Data.Set as Set
import SAT.CNF (type Clause (literals, Clause))
import SAT.Monad (clauseDB, getClauseDB, type SolverM)
import SAT.Optimisers (unitPropagateM)
import Data.Bits (xor)

preprocess :: SolverM (Maybe Clause)
preprocess = do
  -- m <- getAssignment
  clauseDB <- getClauseDB
  let clauses = nub $ map (\c -> c {literals = nub $ literals c}) clauseDB
  modify \s -> s {clauseDB = clauses}
  unitPropagateM >>= \case
    Just c -> return $ pure c
    Nothing -> do
      -- subsumption
      -- eliminateLiteralsM
      return Nothing

-- other preprocessing steps
-- 1. subsumption
-- 2. variable elimination

-- for if i dont treat literals as a var or its negation, instead even or odd
-- isRedundant :: Clause -> Bool
-- isRedundant Clause {literals} = any (\l -> (l xor 1) `elem` literals) literals

subsumption :: SolverM ()
subsumption = do
  clauses <- getClauseDB
  let sortedClauses = sortOn (length . literals) clauses
      subsumed = filterNonSubsumed sortedClauses
  modify \s -> s {clauseDB = subsumed}
  where
    filterNonSubsumed :: [Clause] -> [Clause]
    filterNonSubsumed [] = []
    filterNonSubsumed (c : cs) =
      c : filterNonSubsumed (filter (not . isSubsumed c) cs)

    isSubsumed :: Clause -> Clause -> Bool
    isSubsumed c1 c2 =
      all (`Set.member` c2Set) $ literals c1
      where
        c2Set = Set.fromList $ literals c2
