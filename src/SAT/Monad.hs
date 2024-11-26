{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DerivingStrategies #-}

module SAT.Monad (SolverM, SolverState(..), SolverLog, Trail, Reason, ImplicationGraph, WatchedLiterals, getAssignment, getTrail, getImplicationGraph, getWatchedLiterals, getDecisionLevel, getVSIDS, getPartialAssignment, logM, ifM, guardM, notM, getLearnedClauses, learn, cnfWithLearnedClauses, increaseDecisionLevel, ClauseState(..)) where

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

-- | The implication graph (maybe turn into a more explicit graph).
type ImplicationGraph = IntMap (Literal, Maybe Reason, DecisionLevel)

-- | Watched literals (literals in clauses that are being watched).

-- | Clauses learnt from conflicts (CDCL).
type LearnedClauses = [Clause]

data ClauseState = ClauseState {
  original :: Clause,
  current :: Maybe Clause, -- ^ The clause after resolution.
  watched :: [Literal]
} deriving stock (Show)

type WatchedLiterals = IntMap [ClauseState]

type ClauseDB = [ClauseState]

-- | The solver state.
-- Contains information for solving and any optimisations.
data SolverState = SolverState {
  assignment :: Assignment,
  trail :: Trail,
  implicationGraph :: ImplicationGraph,
  watchedLiterals :: WatchedLiterals,
  decisionLevel :: DecisionLevel,
  clauseDB :: ClauseDB,
  vsids :: VSIDS,
  partial :: CNF,
  learnedClauses :: LearnedClauses
} deriving stock (Show)



-- | The solver log.
type SolverLog = [String]
-- | The solver monad.
type SolverM = RWST CNF SolverLog SolverState Maybe


-- | State methods

-- | Gets the assignment.
getAssignment :: SolverM Assignment
getAssignment = gets assignment
{-# INLINEABLE getAssignment #-}

-- | Gets the trail.
getTrail :: SolverM Trail
getTrail = gets trail
{-# INLINEABLE getTrail #-}

-- | Gets the implication graph.
getImplicationGraph :: SolverM ImplicationGraph
getImplicationGraph = gets implicationGraph
{-# INLINEABLE getImplicationGraph #-}

-- | Gets the watched literals.
getWatchedLiterals :: SolverM WatchedLiterals
getWatchedLiterals = gets watchedLiterals
{-# INLINEABLE getWatchedLiterals #-}

-- | Gets the decision level.
getDecisionLevel :: SolverM DecisionLevel
getDecisionLevel = gets decisionLevel
{-# INLINEABLE getDecisionLevel #-}

-- | Gets the VSIDS.
getVSIDS :: SolverM VSIDS
getVSIDS = gets vsids
{-# INLINEABLE getVSIDS #-}

-- | Gets the partial assignment.
getPartialAssignment :: SolverM CNF
getPartialAssignment = gets partial
{-# INLINEABLE getPartialAssignment #-}

getLearnedClauses :: SolverM LearnedClauses
getLearnedClauses = gets learnedClauses
{-# INLINEABLE getLearnedClauses #-}

learn :: Clause -> SolverM ()
learn clause = modify $ \s -> s { learnedClauses = clause : learnedClauses s}
{-# INLINEABLE learn #-}

cnfWithLearnedClauses :: SolverM CNF
cnfWithLearnedClauses = do
  lc <- getLearnedClauses
  CNF clauses <- ask
  return $ CNF $ clauses ++ lc
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
increaseDecisionLevel = modify $ \s -> s { decisionLevel = decisionLevel s + 1}
{-# INLINEABLE increaseDecisionLevel #-}