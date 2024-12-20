{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module SAT.CDCL (backtrack, analyseConflict) where

import Control.Monad.State.Strict (modify, get)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (partition)
import SAT.CNF (type Clause, varOfLiteral, type DecisionLevel)
import SAT.Monad (type SolverM, getAssignment, getImplicationGraph, getTrail, type SolverState(..))


-- | Backtracks to a given decision level.
backtrack :: DecisionLevel -> SolverM ()
backtrack dl = do
  trail <- getTrail
  assignments <- getAssignment
  ig <- getImplicationGraph

  let (trail', toRemove) = partition (\(_, dl', _) -> dl' < dl) trail
      toRemoveKeys = IntSet.fromList $ map (\(l, _, _) -> l) toRemove
      assignments' = IntMap.filterWithKey (\k _ -> k `IntSet.notMember` toRemoveKeys) assignments
      ig' = IntMap.filterWithKey (\k _ -> k `IntSet.notMember` toRemoveKeys) ig
  modify \s -> s {trail = trail', assignment = assignments', implicationGraph = ig', decisionLevel = dl}

-- partialAssignmentM

{-
def resolve(a: Clause, b: Clause, x: int) -> Clause:
    """
    The resolution operation
    """
    result = set(a.literals + b.literals) - {Literal(x, True), Literal(x, False)}
    result = list(result)
    return Clause(result)
-}
resolve :: Clause -> Clause -> Int -> Clause
resolve a b x =
  IntSet.toList $ IntSet.fromList (a <> b) IntSet.\\ IntSet.fromList [x, -x]

analyseConflict :: Clause -> SolverM (Clause, DecisionLevel)
analyseConflict conflict = do
  SolverState {decisionLevel, implicationGraph} <- get

  if decisionLevel == 0
    then return (conflict, -1)
    else do
      -- literals = [literal for literal in clause if assignments[literal.variable].dl == assignments.dl]
      let literals =
            map varOfLiteral $
              filter
                ( \l -> maybe False (\(_, _, dl) -> dl == decisionLevel) $ IntMap.lookup (varOfLiteral l) implicationGraph
                )
                conflict

      let loop :: [Int] -> SolverM (Clause, DecisionLevel)
          loop [] = return (conflict, -1)
          loop ls = do
            let implied =
                  filter
                    ( \l -> maybe False (\(_, _, dl) -> dl /= decisionLevel) $ IntMap.lookup (varOfLiteral l) implicationGraph
                    )
                    ls

            case implied of
              [] -> return (conflict, -1)
              (l : _) -> do
                let antecedent = case IntMap.lookup (varOfLiteral l) implicationGraph of
                      Just (_, Just c, _) -> c
                      _ -> []
                let clause' = resolve conflict antecedent $ varOfLiteral l
                let literals' =
                      filter (\l' -> maybe False (\(_, _, dl) -> dl == decisionLevel) $ IntMap.lookup (varOfLiteral l') implicationGraph) clause'
                loop literals'

      (conflict', _) <- loop literals

      let decisionLevels =
            -- uniqueOnly $
            map
              (
                \l -> maybe (-1) (\(_, _, dl) -> dl) $ IntMap.lookup (varOfLiteral l) implicationGraph
              )
              conflict'

      if length decisionLevels <= 1
        then return (conflict', -1)
        else return (conflict', decisionLevels !! (length decisionLevels - 2))