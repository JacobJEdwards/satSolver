{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}

module SAT.Preprocessing (preprocess) where

import Control.Monad.RWS.Strict (modify)
import Data.List (sortOn)
import qualified Data.Set as Set
import SAT.CNF (type Clause)
import SAT.Monad (clauseDB, getClauseDB, type SolverM)
import SAT.Optimisers (unitPropagateM)

preprocess :: SolverM (Maybe Clause)
preprocess = do
  -- m <- getAssignment
  unitPropagateM >>= \case
    Just c -> return $ pure c
    Nothing -> do
      subsumption
      -- eliminateLiteralsM
      return mempty

-- m' <- getAssignment
-- if m == m' then return Nothing else preprocess

-- other preprocessing steps
-- 1. subsumption
-- 2. variable elimination

subsumption :: SolverM ()
subsumption = do
  clauses <- getClauseDB
  let sortedClauses = sortOn length clauses
      subsumed = filterNonSubsumed sortedClauses
  modify \s -> s {clauseDB = subsumed}
  where
    filterNonSubsumed :: [Clause] -> [Clause]
    filterNonSubsumed [] = []
    filterNonSubsumed (c : cs) =
      c : filterNonSubsumed (filter (not . isSubsumed c) cs)

    isSubsumed :: Clause -> Clause -> Bool
    isSubsumed c1 c2 =
      all (`elemSet` c2Set) c1
      where
        c2Set = Set.fromList c2

    elemSet = Set.member
