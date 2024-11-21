{-# LANGUAGE ExplicitNamespaces #-}

module SAT.Monad (SolverM, SolverState, SolverLog, Trail, Reason, ImplicationGraph, WatchedLiterals) where
  
import SAT.CNF (type CNF, type Clause, type Literal, type Assignment, type DecisionLevel)
import Data.IntMap (IntMap)
import SAT.VSIDS (type VSIDS)
import Control.Monad.RWS.Strict (RWST)
  
-- | The trail type (the previous assignments).
type Trail = [(Literal, DecisionLevel, Bool)]
-- | The reason for a conflict.
type Reason = Clause
-- | The implication graph.
type ImplicationGraph = IntMap (Reason, DecisionLevel)
-- | Watched literals (literals in clauses that are being watched).
type WatchedLiterals = IntMap [Clause]

-- | The solver state.
type SolverState = (Assignment, Trail, ImplicationGraph, WatchedLiterals, DecisionLevel, VSIDS)
type SolverLog = [String]
-- | The solver monad.
type SolverM = RWST CNF SolverLog SolverState Maybe

