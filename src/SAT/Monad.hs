{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

module SAT.Monad (type SolverM, getPropagationStack, savePhase, nextPhase, type SolverState (..), type SolverLog, type Trail, type Reason, type ClauseDB, type ImplicationGraph, type WatchedLiterals (..), getAssignment, getTrail, getImplicationGraph, getWatchedLiterals, getDecisionLevel, getVSIDS, logM, ifM, guardM, notM, increaseDecisionLevel, getClauseDB) where

import Control.Monad (guard, type MonadPlus)
import Control.Monad.RWS.Strict (modify', tell, type RWST, MonadState (get))
import Control.Monad.State.Strict (gets)
import Control.Parallel.Strategies (type NFData)
import Data.Bool (bool)
import Data.IntMap.Strict (type IntMap)
import Data.IntSet (type IntSet)
import Data.Sequence (type Seq)
import GHC.Generics (type Generic)
import SAT.CNF (type CNF, type Clause, type DecisionLevel, type Literal, varOfLiteral)
import SAT.VSIDS (type VSIDS)
import SAT.Assignment (type Assignment)
import Data.IntMap.Strict qualified as IntMap
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Storable qualified as VS
import Data.Vector qualified as V

-- | The trail type (the previous assignments).
type Trail = [(Literal, DecisionLevel, Bool)]

-- | The reason for a conflict or assignment.
type Reason = Clause

-- | The implication graph (maybe turn into a more explicit graph).
type ImplicationGraph = IntMap (Literal, Maybe Reason, DecisionLevel)

-- | Watched literals (literals in clauses that are being watched).
newtype WatchedLiterals = WatchedLiterals (V.Vector IntSet)
  deriving stock (Generic)

deriving anyclass instance NFData WatchedLiterals

-- | The clause database.
type ClauseDB = Seq Clause

type SavedPhases = IntMap Bool

-- | The solver state.
-- Contains information for solving and any optimisations.
data SolverState = SolverState
  { assignment :: !Assignment,
    trail :: !Trail,
    implicationGraph :: !ImplicationGraph,
    watchedLiterals :: !WatchedLiterals,
    decisionLevel :: !DecisionLevel,
    clauseDB :: !ClauseDB,
    vsids :: !(VSIDS Double),
    variables :: !IntSet,
    propagationStack :: ![(Literal, Bool, Maybe Reason)],
    lubyCount :: !Int,
    lubyThreshold :: !Int,
    savedPhases :: !SavedPhases
  }
  deriving stock (Generic)

-- | The solver log.
type SolverLog = [String]

-- | The solver monad.
type SolverM = RWST CNF SolverLog SolverState Maybe

-- | State methods

-- | Gets the propagation stack.
getPropagationStack :: SolverM [(Literal, Bool, Maybe Reason)]
getPropagationStack = gets propagationStack
{-# INLINEABLE getPropagationStack #-}

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
getVSIDS :: SolverM (VSIDS Double)
getVSIDS = gets vsids
{-# INLINEABLE getVSIDS #-}

getClauseDB :: SolverM ClauseDB
getClauseDB = gets clauseDB
{-# INLINEABLE getClauseDB #-}

getSavedPhases :: SolverM SavedPhases
getSavedPhases = gets savedPhases
{-# INLINEABLE getSavedPhases #-}

savePhase :: Literal -> Bool -> SolverM ()
savePhase lit phase = modify' \s -> s {savedPhases = IntMap.insert (varOfLiteral lit) phase $ savedPhases s}

nextPhase :: Literal -> SolverM Bool
nextPhase lit = do
  not . IntMap.findWithDefault False (varOfLiteral lit) <$> getSavedPhases

{-
def add_learnt_clause(formula, clause, assignments, lit2clauses, clause2lits):
    formula.clauses.append(clause)
    for lit in sorted(clause, key=lambda lit: -assignments[lit.variable].dl):
        if len(clause2lits[clause]) < 2:
            clause2lits[clause].append(lit)
            lit2clauses[lit].append(clause)
        else:
            break
-}

-- | Logs a message.
logM :: String -> SolverM ()
logM = tell . pure
{-# INLINEABLE logM #-}

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM p t f = p >>= bool f t
{-# INLINEABLE ifM #-}

guardM :: (MonadPlus m) => m Bool -> m ()
guardM = (>>= guard)
{-# INLINEABLE guardM #-}

notM :: (Monad m) => m Bool -> m Bool
notM = fmap not
{-# INLINEABLE notM #-}

increaseDecisionLevel :: SolverM ()
increaseDecisionLevel = modify' \s -> s {decisionLevel = decisionLevel s + 1}
{-# INLINEABLE increaseDecisionLevel #-}
