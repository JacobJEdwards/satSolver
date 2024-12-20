{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE BlockArguments #-}

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
    checkValue,
    findFreeVariable,
    getSolutions,
    bruteForce,
    withUnitPropagation,
    withPureLiteralElimination,
    withPureLiteralAndUnitPropagation,
    pureLitOnlyAsPreprocess,
    literalEvery100,
    withTautologyElimination,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.RWS.Strict (get, modify)
import Control.Monad.RWS.Strict qualified as RWST
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (listToMaybe, isJust)
import SAT.CNF (initAssignment, toCNF, type Assignment, type CNF (CNF), type Clause, type Literal)
import SAT.Expr (Expr, type Solutions)
import SAT.Monad (SolverM, SolverState (SolverState, assignment, clauseDB, decisionLevel, implicationGraph, lubyCount, lubyThreshold, propagationStack, trail, variables, vsids, watchedLiterals), getAssignment, ifM, increaseDecisionLevel, initWatchedLiterals, learn)
import SAT.Optimisers (addDecision, adjustScoresM, assign, assignM, collectLiterals, collectLiteralsToSet, decayM, eliminateLiterals, pickLiteralM, substitute, unitPropagate, unitPropagateM)
import SAT.CDCL (analyseConflict, backtrack)
import SAT.Preprocessing (preprocess)
import SAT.Restarts (computeNextLubyThreshold, increaseLubyCount)
import SAT.VSIDS (initVSIDS)

-- | Initializes the solver state.
initState :: CNF -> SolverState
initState cnf@(CNF clauses) =
  SolverState
    { assignment = initAssignment $ collectLiteralsToSet cnf,
      trail = mempty,
      implicationGraph = mempty,
      watchedLiterals = initWatchedLiterals cnf,
      decisionLevel = 0,
      vsids = initVSIDS cnf,
      clauseDB = clauses,
      variables = collectLiteralsToSet cnf,
      propagationStack = mempty,
      lubyCount = 0,
      lubyThreshold = 1
    }

-- | Finds a free variable at random.
findFreeVariable :: CNF -> Maybe Literal
findFreeVariable = listToMaybe . collectLiterals
{-# INLINEABLE findFreeVariable #-}

-- | Checks if a value is in the solutions.
--
-- >>> checkValue (IntSet.fromList [1, 2, 3]) 2
-- True
checkValue :: Solutions -> Literal -> Bool
checkValue = flip IntSet.member
{-# INLINEABLE checkValue #-}

-- | Converts an assignment to a set of solutions.
solutionsFromAssignment :: Assignment -> Solutions
solutionsFromAssignment = IntMap.keysSet . IntMap.filter id
{-# INLINEABLE solutionsFromAssignment #-}

solutions :: SolverM Solutions
solutions = solutionsFromAssignment <$> getAssignment

-- i think this is close, slightly wrong, but close ! (maybe)
getSolutions :: CNF -> Maybe Solutions
getSolutions !cnf' = do
  (solutions', _) <- RWST.evalRWST run cnf' $ initState cnf'
  solutions'
  where
    run :: SolverM (Maybe Solutions)
    run = do
      preprocess >>= \case
        Just _ -> return mempty -- failure from the start
        Nothing -> solver

    solver :: SolverM (Maybe Solutions)
    solver = do
      ifM allVariablesAssigned (return <$> solutions) branch

    branch :: SolverM (Maybe Solutions)
    branch = pickLiteralM >>= try

    try :: Literal -> SolverM (Maybe Solutions)
    try c = tryAssign c True <|> tryAssign c False

    tryAssign :: Literal -> Bool -> SolverM (Maybe Solutions)
    tryAssign c v = do
      increaseDecisionLevel
      addDecision c
      assignM c v
      propagate

    propagate :: SolverM (Maybe Solutions)
    propagate = do
      unitPropagateM >>= \case
        Just c -> handleConflict c
        Nothing -> solver

    handleConflict :: Clause -> SolverM (Maybe Solutions)
    handleConflict c = do
      (clause, dl) <- analyseConflict c
      if dl < 0
        then return mempty
        else do
          decayM
          adjustScoresM clause
          learn clause
          increaseLubyCount
          ifM shouldRestart restart do
            backtrack dl
            solver

    shouldRestart :: SolverM Bool
    shouldRestart = do
      SolverState {lubyCount} <- get
      let nextThreshold = computeNextLubyThreshold $ lubyCount + 1
      return $ lubyCount >= nextThreshold

    restart :: SolverM (Maybe Solutions)
    restart = do
      modify \s -> s {assignment = mempty, decisionLevel = 0, trail = mempty, implicationGraph = mempty}
      solver
{-# INLINEABLE getSolutions #-}

-- | Checks if a CNF is satisfiable.
satisfiable :: CNF -> Bool
satisfiable = isJust . getSolutions
{-# INLINEABLE satisfiable #-}

bruteForce :: Expr Int -> Maybe Solutions
bruteForce expr = let cnf = toCNF expr in go cnf mempty
  where
    go :: CNF -> Assignment -> Maybe Solutions
    go cnf m = case findFreeVariable cnf of
      Nothing -> if isSat cnf then Just (solutionsFromAssignment m) else Nothing
      Just c -> try c True <|> try c False
      where
        try :: Literal -> Bool -> Maybe Solutions
        try c v = go (substitute c v cnf) (assign m c v)

        isSat :: CNF -> Bool
        isSat (CNF clauses) = null clauses
{-# INLINEABLE bruteForce #-}

withUnitPropagation :: Expr Int -> Maybe Solutions
withUnitPropagation expr = let cnf = toCNF expr in go cnf mempty
  where
    go :: CNF -> Assignment -> Maybe Solutions
    go cnf m = case findFreeVariable cnf' of
      Nothing -> if isSat cnf' then Just (solutionsFromAssignment m') else Nothing
      Just c -> try c True <|> try c False
      where
        try :: Literal -> Bool -> Maybe Solutions
        try c v = go (substitute c v cnf') (assign m' c v)

        isSat :: CNF -> Bool
        isSat (CNF clauses) = null clauses

        cnf' :: CNF
        (m', cnf') = unitPropagate cnf m 0
{-# INLINEABLE withUnitPropagation #-}

withPureLiteralElimination :: Expr Int -> Maybe Solutions
withPureLiteralElimination expr = let cnf = toCNF expr in go cnf mempty
  where
    go :: CNF -> Assignment -> Maybe Solutions
    go cnf m = case findFreeVariable cnf' of
      Nothing -> if isSat cnf' then Just (solutionsFromAssignment m') else Nothing
      Just c -> try c True <|> try c False
      where
        try :: Literal -> Bool -> Maybe Solutions
        try c v = go (substitute c v cnf') (assign m' c v)

        isSat :: CNF -> Bool
        isSat (CNF clauses) = null clauses

        cnf' :: CNF
        (m', cnf') = eliminateLiterals cnf m
{-# INLINEABLE withPureLiteralElimination #-}

withPureLiteralAndUnitPropagation :: Expr Int -> Maybe Solutions
withPureLiteralAndUnitPropagation expr = let cnf = toCNF expr in go cnf mempty
  where
    go :: CNF -> Assignment -> Maybe Solutions
    go cnf m = case findFreeVariable cnf'' of
      Nothing -> if isSat cnf'' then Just (solutionsFromAssignment m'') else Nothing
      Just c -> try c True <|> try c False
      where
        try :: Literal -> Bool -> Maybe Solutions
        try c v = go (substitute c v cnf'') (assign m'' c v)

        isSat :: CNF -> Bool
        isSat (CNF clauses) = null clauses

        cnf' :: CNF
        (m', cnf') = eliminateLiterals cnf m

        cnf'' :: CNF
        (m'', cnf'') = unitPropagate cnf' m' 0
{-# INLINEABLE withPureLiteralAndUnitPropagation #-}

pureLitOnlyAsPreprocess :: Expr Int -> Maybe Solutions
pureLitOnlyAsPreprocess expr = go cnf' m
  where
    cnf = toCNF expr
    (m, cnf') = eliminateLiterals cnf mempty

    go :: CNF -> Assignment -> Maybe Solutions
    go cnf'' m' = case findFreeVariable cnf'' of
      Nothing -> if isSat cnf'' then Just (solutionsFromAssignment m'') else Nothing
      Just c -> try c True <|> try c False
      where
        try :: Literal -> Bool -> Maybe Solutions
        try c v = go (substitute c v cnf''') (assign m'' c v)

        isSat :: CNF -> Bool
        isSat (CNF clauses) = null clauses

        (m'', cnf''') = unitPropagate cnf'' m' 0
{-# INLINEABLE pureLitOnlyAsPreprocess #-}

literalEvery100 :: Expr Int -> Maybe Solutions
literalEvery100 expr = go cnf' m 0
  where
    cnf = toCNF expr
    (m, cnf') = eliminateLiterals cnf mempty

    go :: CNF -> Assignment -> Int -> Maybe Solutions
    go cnf'' m' i = case findFreeVariable cnf'''' of
      Nothing -> if isSat cnf'''' then Just (solutionsFromAssignment m''') else Nothing
      Just c -> try c True <|> try c False
      where
        try :: Literal -> Bool -> Maybe Solutions
        try c v = go (substitute c v cnf'''') (assign m'' c v) (i + 1)

        isSat :: CNF -> Bool
        isSat (CNF clauses) = null clauses

        (m'', cnf''') = unitPropagate cnf'' m' 0
        (m''', cnf'''') = if i `mod` 100 == 0 then eliminateLiterals cnf''' m'' else (m'', cnf''')
{-# INLINEABLE literalEvery100 #-}

withTautologyElimination :: Expr Int -> Maybe Solutions
withTautologyElimination expr = go init' m''
  where
    cnf'' = toCNF expr
    (m'', i) = eliminateLiterals cnf'' mempty
    init' = removeTautologies i

    go :: CNF -> Assignment -> Maybe Solutions
    go cnf m = case findFreeVariable cnf' of
      Nothing -> if isSat cnf' then Just (solutionsFromAssignment m') else Nothing
      Just c -> try c True <|> try c False
      where
        try :: Literal -> Bool -> Maybe Solutions
        try c v = go (substitute c v cnf') (assign m' c v)

        isSat :: CNF -> Bool
        isSat (CNF clauses) = null clauses

        cnf' :: CNF
        (m', cnf') = unitPropagate cnf m 0
{-# INLINEABLE withTautologyElimination #-}

allAssignments :: Assignment -> IntSet
allAssignments = IntMap.keysSet

allVariablesAssigned :: SolverM Bool
allVariablesAssigned = do
  SolverState {variables, assignment} <- get
  return $ IntSet.null $ variables `IntSet.difference` allAssignments assignment

removeTautologies :: CNF -> CNF
removeTautologies (CNF cs) = CNF $ filter (not . isTautology) cs
  where
    isTautology :: Clause -> Bool
    isTautology c = any (\l -> IntSet.member (negate l) (IntSet.fromList c)) c
