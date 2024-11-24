{-# LANGUAGE ExplicitNamespaces #-}

module SAT.Monad (SolverM, SolverState, SolverLog, Trail, Reason, ImplicationGraph, WatchedLiterals, getAssignment, getTrail, getImplicationGraph, getWatchedLiterals, getDecisionLevel, getVSIDS, getPartialAssignment, logM, ifM, guardM, notM, getLearnedClauses, learn, cnfWithLearnedClauses, increaseDecisionLevel) where

import SAT.CNF (type CNF (CNF), type Clause, type Literal, type Assignment, type DecisionLevel)
import Data.IntMap (IntMap)
import SAT.VSIDS (type VSIDS)
import Control.Monad.RWS.Strict (RWST, tell, modify, MonadReader (ask))
import Control.Monad.State.Strict (gets)
import Data.Bool (bool)
import Control.Monad (MonadPlus, guard)

-- | The trail type (the previous assignments).
type Trail = [(Literal, DecisionLevel, Bool)]
-- | The reason for a conflict.
type Reason = Clause
-- | The implication graph.
type ImplicationGraph = IntMap (Literal, Maybe Reason, DecisionLevel)
-- | Watched literals (literals in clauses that are being watched).
type WatchedLiterals = IntMap [Clause]

type LearnedClauses = [Clause]

-- | The solver state.
type SolverState = (Assignment, Trail, ImplicationGraph, WatchedLiterals, DecisionLevel, VSIDS, CNF, LearnedClauses) -- partial assignment at end

-- | The solver log.
type SolverLog = [String]
-- | The solver monad.
type SolverM = RWST CNF SolverLog SolverState Maybe


-- | State methods

-- | Gets the assignment.
getAssignment :: SolverM Assignment
getAssignment = gets $ \(a, _, _, _, _, _, _, _) -> a
{-# INLINEABLE getAssignment #-}

-- | Gets the trail.
getTrail :: SolverM Trail
getTrail = gets $ \(_, t, _, _, _, _, _, _) -> t
{-# INLINEABLE getTrail #-}

-- | Gets the implication graph.
getImplicationGraph :: SolverM ImplicationGraph
getImplicationGraph = gets $ \(_, _, ig, _, _, _, _, _) -> ig
{-# INLINEABLE getImplicationGraph #-}

-- | Gets the watched literals.
getWatchedLiterals :: SolverM WatchedLiterals
getWatchedLiterals = gets $ \(_, _, _, wl, _, _,_, _) -> wl
{-# INLINEABLE getWatchedLiterals #-}

-- | Gets the decision level.
getDecisionLevel :: SolverM DecisionLevel
getDecisionLevel = gets $ \(_, _, _, _, dl, _, _,_) -> dl
{-# INLINEABLE getDecisionLevel #-}

-- | Gets the VSIDS.
getVSIDS :: SolverM VSIDS
getVSIDS = gets $ \(_, _, _, _, _, vsids, _,_) -> vsids
{-# INLINEABLE getVSIDS #-}

-- | Gets the partial assignment.
getPartialAssignment :: SolverM CNF
getPartialAssignment = gets $ \(_, _, _, _, _, _, cnf, _) -> cnf
{-# INLINEABLE getPartialAssignment #-}

getLearnedClauses :: SolverM LearnedClauses
getLearnedClauses = gets $ \(_, _, _, _, _, _, _, lc) -> lc
{-# INLINEABLE getLearnedClauses #-}

learn :: Clause -> SolverM ()
learn clause = modify $ \(a, b, c, d, e, f, g, lc) -> (a, b, c, d, e, f, g, clause:lc)
{-# INLINEABLE learn #-}

cnfWithLearnedClauses :: SolverM CNF
cnfWithLearnedClauses = do
  learnedClauses <- getLearnedClauses
  -- get cnf from reader monad
  CNF clauses <- ask
  return $ CNF $ clauses ++ learnedClauses
{-# INLINEABLE cnfWithLearnedClauses #-}

-- | Logs a message.
logM :: String -> SolverM ()
logM = tell . pure
{-# INLINEABLE logM #-}

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p t f = p >>= bool f t

guardM :: MonadPlus m => m Bool -> m ()
guardM = (>>= guard)

notM :: Monad m => m Bool -> m Bool
notM = fmap not

increaseDecisionLevel :: SolverM ()
increaseDecisionLevel = modify $ \(a, b, c, d, e, f, g, h) -> (a, b, c, d, e + 1, f, g, h)
{-# INLINEABLE increaseDecisionLevel #-}