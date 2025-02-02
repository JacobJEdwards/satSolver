{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

-- TODO: check luby restarts
-- TODO: check more preprocessing
-- TODO: watch literals
-- TODO: dsa

-- |
-- Module      : SAT.Solver
-- Description : Exports the SAT solver module.
-- https://kienyew.github.io/CDCL-SAT-Solver-from-Scratch/Two-Watched-Literals.html
-- https://www.cs.upc.edu/~oliveras/LAI/cdcl.pdf
-- simplify for now, don't use partials, don't worry about inefficiencies
-- that is what watch literals aim to solve
-- or i could have my partial list of maybe clause, which i can index on the main clauses to get them
-- but must compare that to watched literals when i implement that
-- watched literals can also be checked for tautolofy
-- should wathced literals be stored as the abs value or either ?
--
-- so for now, when finding units just compare the main cnf against the assignment
-- follow the tutorial (but with ig)
-- experiment with the use of pure literals, not sure how they fit in with the watched literals (maybe tautlogies could be useful too)
--
-- maybe worth dragging the cnf around in state to add learned clauses iwht little hassle
-- consider not using vsids for now, just bare minimum
-- biggest issue is that i need the conflicting clause to learn from it, hence lack of partials
-- another issue is if my solver is dramatically more inefficient due to partials lacking, how can i know if cdcl is working ??
-- i suppose i check the learned clauses against other solvers
--
-- simplify simplify simpify
--
-- issue with using partial assignment to check for isSat is learned clauses
module SAT.Solver
  ( satisfiable,
    type Solutions,
    findFreeVariable,
    getSolutions,
    satisfied,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.RWS.Strict (get, modify')
import Control.Monad.RWS.Strict qualified as RWST
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (foldl')
import Data.Maybe (isJust, listToMaybe)
import Data.Sequence qualified as Seq
import Debug.Trace (traceM)
import SAT.CDCL (analyseConflict, backtrack)
import SAT.CNF (varOfLiteral, type CNF (CNF), type Clause (Clause, literals, watched), type Literal)
import SAT.Assignment (type Assignment, allAssignments, literalValue, type Solutions, solutionsFromAssignment, initAssignment)
import SAT.Monad (Reason, getAssignment, getClauseDB, getWatchedLiterals, ifM, increaseDecisionLevel, type SolverM, type SolverState (SolverState, assignment, clauseDB, decisionLevel, implicationGraph, lubyCount, lubyThreshold, propagationStack, trail, variables, vsids, watchedLiterals), type WatchedLiterals (WatchedLiterals), savedPhases, nextPhase, savePhase, getDecisionLevel)
import SAT.Optimisers (addDecision, adjustScoresM,collectLiterals, collectLiteralsToSet, decayM, pickLiteralM, unitPropagateM)
import SAT.Preprocessing (preprocess)
import SAT.Restarts (computeNextLubyThreshold, increaseLubyCount)
import SAT.VSIDS (initVSIDS)
import SAT.WL (initClauseWatched, initWatchedLiterals)
import Utils (unstableHashNub)

-- | Initializes the solver state.
initState :: CNF -> SolverState
initState cnf@(CNF clauses) =
  SolverState
    { assignment = initAssignment $ collectLiteralsToSet cnf,
      trail = mempty,
      implicationGraph = mempty,
      watchedLiterals = initWatchedLiterals db,
      decisionLevel = 0,
      vsids = initVSIDS cnf,
      clauseDB = db,
      variables = collectLiteralsToSet cnf,
      propagationStack = initialPropagationStack watchedClauses,
      lubyCount = 0,
      lubyThreshold = 1,
      savedPhases = mempty
    }
  where
    !watchedClauses = unstableHashNub $ map initClauseWatched clauses

    !db = Seq.fromList watchedClauses

initialPropagationStack :: [Clause] -> [(Literal, Bool, Maybe Reason)]
initialPropagationStack =
  foldl' (\acc c@(Clause {literals, watched = (a, b)}) -> if a == b then (abs $ literals !! a, literals !! a > 0, Just c) : acc else acc) mempty
{-# INLINEABLE initialPropagationStack #-}

learn :: [Literal] -> SolverM ()
learn literals = do
  let (a, b) = getInitialWatched literals
  let clause = Clause {literals, watched = (a, b)}

  WatchedLiterals lits <- getWatchedLiterals
  clauseDB <- getClauseDB

  if a == b
    then modify' \s -> s {propagationStack = (varOfLiteral $ literals !! a, literals !! a > 0, Just clause) : propagationStack s} -- unit clause
    else do 
      let newClauseDB = clauseDB Seq.|> clause
      let aLits = IntMap.findWithDefault mempty (varOfLiteral $ literals !! a) lits
      let bLits = IntMap.findWithDefault mempty (varOfLiteral $ literals !! b) lits
      let aLits' = IntSet.insert (Seq.length newClauseDB - 1) aLits
      let bLits' = IntSet.insert (Seq.length newClauseDB - 1) bLits
      let newWL = IntMap.insert (varOfLiteral $ literals !! a) aLits' $ IntMap.insert (varOfLiteral $ literals !! b) bLits' lits
      modify' \s -> s {clauseDB = newClauseDB, watchedLiterals = WatchedLiterals newWL}
  where
    getInitialWatched :: [Literal] -> (Literal, Literal)
    getInitialWatched clause =
      case clause of
        [_] -> (0, 0)
        _ : _ : _ -> (0, 1)
        _ -> error "Empty clause"
{-# INLINEABLE learn #-}

-- | Finds a free variable at random.
findFreeVariable :: CNF -> Maybe Literal
findFreeVariable = listToMaybe . collectLiterals
{-# INLINEABLE findFreeVariable #-}


solutions :: SolverM Solutions
solutions = solutionsFromAssignment <$> getAssignment

getSolutions :: CNF -> Maybe Solutions
getSolutions !cnf' = do
  (solutions', _) <- RWST.evalRWST run cnf' $ initState cnf'
  solutions'
  where
    run :: SolverM (Maybe Solutions)
    run = do
      clauseDb <- getClauseDB

      guard $ not $ null clauseDb
      guard $ not $ any (null . literals) clauseDb

      solver

    solver :: SolverM (Maybe Solutions)
    solver = do
      conflict <- unitPropagateM
      case conflict of
        Just c -> do 
          handleConflict c
        Nothing -> do
          ifM allVariablesAssigned (return <$> solutions) branch

    branch :: SolverM (Maybe Solutions)
    branch = pickLiteralM >>= try

    try :: Literal -> SolverM (Maybe Solutions)
    try c = do
      phase <- nextPhase c
      savePhase c phase
      tryAssign c phase <|> tryAssign c (not phase)

    tryAssign :: Literal -> Bool -> SolverM (Maybe Solutions)
    tryAssign c val = do
      increaseDecisionLevel
      addDecision c val
      solver

    handleConflict :: Clause -> SolverM (Maybe Solutions)
    handleConflict !c = do
      (clause, dl) <- analyseConflict c
      if dl < 0
        then do 
          traceM "Unsatisfiable"
          return Nothing
        else do
          decayM
          adjustScoresM clause
          learn clause
          increaseLubyCount
          -- ifM shouldRestart restart do
          backtrack dl
          solver

    shouldRestart :: SolverM Bool
    shouldRestart = do
      traceM "Checking restart"
      SolverState {lubyCount} <- get
      let nextThreshold = computeNextLubyThreshold $ lubyCount + 1
      return $ lubyCount >= nextThreshold

    restart :: SolverM (Maybe Solutions)
    restart = do
      traceM "Restarting"
      modify' \s -> s {assignment = mempty, decisionLevel = 0, trail = mempty, implicationGraph = mempty, propagationStack = mempty}
      solver
{-# INLINEABLE getSolutions #-}

-- | Checks if the solver is satisfied.
satisfied :: SolverM Bool
satisfied = do
  assignment <- getAssignment
  all (clauseIsSatisfied assignment) <$> getClauseDB

-- | Checks if a clause is satisfied.
clauseIsSatisfied :: Assignment -> Clause -> Bool
clauseIsSatisfied m Clause {watched = (a, b)} = literalValue m a == Just True || literalValue m b == Just True
{-# INLINEABLE clauseIsSatisfied #-}

-- | Checks if a CNF is satisfiable.
satisfiable :: CNF -> Bool
satisfiable = isJust . getSolutions
{-# INLINEABLE satisfiable #-}


-- allVariablesAssigned :: SolverM Bool
-- allVariablesAssigned = do
--   SolverState {variables, assignment} <- get

--   traceM $ "Variables: " <> (show $ IntSet.size variables)
--   traceM $ "Assignments: " <> (show $ IntSet.size $ allAssignments assignment)

--   return $ IntSet.size (allAssignments assignment) >= IntSet.size variables
-- {-# INLINEABLE allVariablesAssigned #-}

allVariablesAssigned :: SolverM Bool
allVariablesAssigned = do
  SolverState {variables, assignment} <- get
  return $ IntSet.null $ variables `IntSet.difference` allAssignments assignment

-- return $ Stack.size trail >= IntSet.size variables
