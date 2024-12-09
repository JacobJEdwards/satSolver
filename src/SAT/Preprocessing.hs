{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitNamespaces #-}
module SAT.Preprocessing (preprocess) where

import SAT.Monad(type SolverM)
import SAT.CNF(type Clause)
import SAT.Optimisers(unitPropagateM)

preprocess :: SolverM (Maybe Clause)
preprocess = do
  -- m <- getAssignment
  unitPropagateM >>= \case
    Just c -> return (Just c)
    Nothing -> do
      -- eliminateLiteralsM
      return Nothing
      -- m' <- getAssignment
      -- if m == m' then return Nothing else preprocess