{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ImportQualifiedPost #-}

module SAT.Preprocessing (preprocess) where

import Control.Monad.RWS.Strict (modify)
import Data.List (sortOn)
import Data.Set qualified as Set
import SAT.CNF (type Clause (literals))
import SAT.Monad (clauseDB, getClauseDB, type SolverM)
import SAT.Optimisers (unitPropagateM)
import Data.Sequence qualified as Seq
import Data.Sequence (type Seq)

preprocess :: SolverM (Maybe Clause)
preprocess = do
  -- m <- getAssignment
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
  let sortedClauses = Seq.sortOn (length . literals) clauses
      subsumed = filterNonSubsumed sortedClauses
  modify \s -> s {clauseDB = subsumed}
  where
    filterNonSubsumed :: Seq Clause -> Seq Clause
    filterNonSubsumed Seq.Empty = Seq.empty
    filterNonSubsumed (c Seq.:<| cs) =
      c Seq.:<| filterNonSubsumed (Seq.filter (not . isSubsumed c) cs)

    isSubsumed :: Clause -> Clause -> Bool
    isSubsumed c1 c2 =
      all (`Set.member` c2Set) $ literals c1
      where
        c2Set = Set.fromList $ literals c2
