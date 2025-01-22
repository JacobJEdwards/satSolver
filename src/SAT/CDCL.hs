{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module SAT.CDCL (backtrack, analyseConflict) where

import Control.Monad.State.Strict (get, modify')
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (partition)
import SAT.CNF (type Literal, varOfLiteral, type Clause (Clause, literals), type DecisionLevel)
import SAT.Monad (getAssignment, getImplicationGraph, getTrail, type SolverM, type SolverState (SolverState, assignment, decisionLevel, implicationGraph, propagationStack, trail), getDecisionLevel)
import Utils (unstableIntNub)
import Debug.Trace (traceM)

-- | Backtracks to a given decision level.
-- backtrack :: DecisionLevel -> SolverM ()
-- backtrack dl = do
--   trail <- getTrail
--   assignments <- getAssignment
--   ig <- getImplicationGraph

--   let popStackUntilDL :: Stack.Stack (Literal, DecisionLevel, Bool) -> IntSet.IntSet -> DecisionLevel -> (Stack.Stack (Literal, DecisionLevel, Bool), IntSet.IntSet)
--       popStackUntilDL stack acc dl' = case Stack.pop stack of
--         Just ((l, dl'', _), t') | dl'' <= dl' ->
--           (t', IntSet.insert (varOfLiteral l) acc)
--         Just ((l, _, _), t') ->
--           popStackUntilDL t' (IntSet.insert (varOfLiteral l) acc) dl'
--         Nothing -> (stack, acc)

--   let (trail', toRemove) = popStackUntilDL trail mempty dl
--       assignments' = IntMap.filterWithKey (\k _ -> k `IntSet.notMember` toRemove) assignments
--       ig' = IntMap.filterWithKey (\k _ -> k `IntSet.notMember` toRemove) ig

--   modify' \s -> s {trail = trail', assignment = assignments', implicationGraph = ig', decisionLevel = dl}

-- | Backtracks to a given decision level.
backtrack :: DecisionLevel -> SolverM ()
backtrack dl = do
  trail <- getTrail
  assignments <- getAssignment
  ig <- getImplicationGraph
  dl' <- getDecisionLevel
  traceM $ "Backtracking to " ++ show dl ++ " from " ++ show dl'


  let (trail', toRemove) = partition (\(_, dl', _) -> dl' <= dl) trail
      toRemoveKeys = IntSet.fromList $ fmap (\(l, _, _) -> varOfLiteral l) toRemove
      assignments' = IntMap.filterWithKey (\k _ -> varOfLiteral k `IntSet.notMember` toRemoveKeys) assignments
      ig' = IntMap.filterWithKey (\k _ -> varOfLiteral k `IntSet.notMember` toRemoveKeys) ig

  modify' \s -> s {trail = trail', assignment = assignments', implicationGraph = ig', decisionLevel = dl, propagationStack = mempty}

resolve :: [Literal] -> [Literal] -> Int -> [Literal]
resolve a b x = IntSet.toList $ IntSet.fromList (a <> b) IntSet.\\ IntSet.fromList [x, -x]

analyseConflict :: Clause -> SolverM ([Literal], DecisionLevel)
analyseConflict (Clause {literals = conflict}) = do
  SolverState {decisionLevel, implicationGraph} <- get

  if decisionLevel == 0
    then return (conflict, -1)
    else do
      let literals =
            varOfLiteral
              <$> filter
                ( \l -> maybe False (\(_, _, dl) -> dl == decisionLevel) $ IntMap.lookup (varOfLiteral l) implicationGraph
                )
                conflict

      let loop :: [Literal] -> SolverM ([Literal], DecisionLevel)
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
                      Just (_, Just (Clause {literals = c}), _) -> c
                      _ -> []
                let clause' = resolve conflict antecedent $ varOfLiteral l
                let literals' =
                      filter (\l' -> maybe False (\(_, _, dl) -> dl == decisionLevel) $ IntMap.lookup (varOfLiteral l') implicationGraph) clause'
                loop literals'

      (conflict', _) <- loop literals

      let decisionLevels =
            unstableIntNub $ 
            map
              ( \l -> maybe (-1) (\(_, _, dl) -> dl) $ IntMap.lookup (varOfLiteral l) implicationGraph
              )
              conflict'

      if length decisionLevels <= 1
        then return (conflict', -1)
        else return (conflict', decisionLevels !! (length decisionLevels - 2))
