{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module SAT.Monad (SolverM, getPropagationStack, SolverState(..), SolverLog, Trail, Reason, ImplicationGraph, WatchedLiterals(..), getAssignment, getTrail, getImplicationGraph, getWatchedLiterals, initWatchedLiterals, getDecisionLevel, getVSIDS, logM, ifM, guardM, notM, learn, increaseDecisionLevel, getClauseDB) where

import SAT.CNF (type CNF (CNF), type Clause, type Literal, type Assignment, type DecisionLevel)
import Data.IntMap (IntMap)
import SAT.VSIDS (type VSIDS)
import Control.Monad.RWS.Strict (RWST, tell, modify)
import Control.Monad.State.Strict (gets)
import Data.Bool (bool)
import Control.Monad (MonadPlus, guard)
import Data.IntSet (IntSet)
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Control.Parallel.Strategies (NFData)

-- | The trail type (the previous assignments).
type Trail = [(Literal, DecisionLevel, Bool)]

-- | The reason for a conflict or assignment.
type Reason = Clause

-- | The implication graph (maybe turn into a more explicit graph).
type ImplicationGraph = IntMap (Literal, Maybe Reason, DecisionLevel)

-- | Watched literals (literals in clauses that are being watched).
data WatchedLiterals = WatchedLiterals {
  literals :: IntMap [Clause],
  clauses :: Map Clause [Literal]
} deriving stock (Show, Eq, Ord, Read, Generic)

deriving anyclass instance NFData WatchedLiterals

initWatchedLiterals :: CNF -> WatchedLiterals
initWatchedLiterals (CNF cs) = do 
  let getWatched clause = do 
        case clause of 
          [l] -> [l]
          l1 : l2 : _ -> [l1, l2]
          _ -> []

  let getDS clause = do 
        let lits = getWatched clause
        let litMap = IntMap.fromList $ map (, [clause]) lits
        let clauseMap = Map.fromList [(clause, lits)]
        (litMap, clauseMap)

  let (l, c) = foldr (\clause (lits, cls) -> do
          let (l', c') = getDS clause
          (IntMap.unionWith (++) lits l', Map.unionWith (++) cls c')
        ) (mempty, mempty) cs

  WatchedLiterals l c

-- | The clause database.
type ClauseDB = [Clause]

type LubyCount = Int

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
  variables :: IntSet,
  propagationStack :: [Literal],
  lubyCount :: LubyCount,
  lubyThreshold :: Int
} deriving stock (Show)


-- | The solver log.
type SolverLog = [String]
-- | The solver monad.
type SolverM = RWST CNF SolverLog SolverState Maybe


-- | State methods

-- | Gets the propagation stack.
getPropagationStack :: SolverM [Literal]
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
getVSIDS :: SolverM VSIDS
getVSIDS = gets vsids
{-# INLINEABLE getVSIDS #-}

getClauseDB :: SolverM ClauseDB
getClauseDB = gets clauseDB
{-# INLINEABLE getClauseDB #-}

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

learn :: Clause -> SolverM ()
learn clause = modify $ \s -> s { clauseDB = clause : clauseDB s }
{-# INLINEABLE learn #-}
-- learn :: Clause -> SolverM ()
-- learn clause = do 
--   learned <- getLearnedClauses
--   clauses <- getClauseDB
--   assignments <- getAssignment
--   let learned' = clause : learned
--   wLits <- getWatchedLiterals

--   let updatedClauses = clause : clauses
--   let sortedLiterals = sortBy (compareDecisionLevel assignments) clause

--   let (watchedLits, _) = splitAt 2 sortedLiterals
--   let updatedWatched = foldl (updateWatched clause) wLits watchedLits

--   modify (\s -> s {
--         clauseDB = updatedClauses,
--         watchedLiterals = updatedWatched,
--         learnedClauses = learned'
--     })
  
--   modify $ \s -> s { learnedClauses = clause : learnedClauses s, clauseDB = clause : clauseDB s }
-- {-# INLINEABLE learn #-}

-- compareDecisionLevel :: Assignment -> Literal -> Literal -> Ordering
-- compareDecisionLevel assignments lit1 lit2 =
--     compare (getDL lit2) (getDL lit1)
--   where
--     getDL lit = fromMaybe 0 $ do
--       (b, dl) <- IntMap.lookup (varOfLiteral lit) assignments
--       if b then Just dl else Nothing

-- -- Update watched literals for a given literal
-- updateWatched :: Clause -> WatchedLiterals -> Literal -> WatchedLiterals
-- updateWatched clause wLits lit =
--     let litMap = literals wLits
--         clauseMap = clauses wLits
--         updatedLitMap = IntMap.insertWith (++) (varOfLiteral lit) [clause] litMap
--     in wLits { literals = updatedLitMap, clauses = Map.insertWith (++) clause [lit] clauseMap }

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