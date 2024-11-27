{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NamedFieldPuns #-}

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
  )
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.RWS.Strict qualified as RWST
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe (listToMaybe)
import SAT.CNF (toCNF, type Assignment, type CNF (CNF), type Clause, type Literal)
import SAT.Expr (Expr, type Solutions)
import SAT.Monad (SolverM, SolverState (..), WatchedLiterals, getAssignment, ifM, learn, increaseDecisionLevel, ClauseState (..), initWatchedLiterals, getVSIDS)
import SAT.Optimisers (assignM, collectLiterals, eliminateLiterals, eliminateLiteralsM, removeTautologiesM, substitute, unitPropagate, unitPropagateM, analyseConflict, backtrack, addDecision, collectLiteralsToSet, decayM, adjustScoresM)
import SAT.VSIDS (initVSIDS, pickLiteral)
import Control.Monad.RWS (get, modify)
import Debug.Trace (trace, traceM)

-- | Initializes the solver state.
initState :: CNF -> SAT.Monad.SolverState
initState cnf@(CNF clauses) =
  SolverState
    { assignment = mempty,
      trail = mempty,
      implicationGraph = mempty,
      watchedLiterals = initWatchedLiterals cnf,
      decisionLevel = 0,
      vsids = initVSIDS cnf,
      clauseDB = clauses,
      partial = cnf,
      learnedClauses = mempty,
      variables = collectLiteralsToSet cnf,
      propagationStack = mempty
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
solutionsFromAssignment = IntMap.keysSet . IntMap.filter fst
{-# INLINEABLE solutionsFromAssignment #-}

solutions :: SolverM Solutions
solutions = solutionsFromAssignment <$> getAssignment

maybeEliminateLiterals :: SolverM ()
maybeEliminateLiterals = do
  SolverState { assignment, partial, decisionLevel } <- get
  when (mod decisionLevel 100 == 0) $ do
    let (m', partial') = eliminateLiterals partial assignment decisionLevel
    modify $ \s -> s { partial = partial', assignment = m' }
    return ()

simplifyM :: SolverM ()
simplifyM = maybeEliminateLiterals
-- removeTautologiesM

preprocess :: SolverM (Maybe Clause)
preprocess = do
  m <- getAssignment
  unitPropagateM >>= \case
    Just c -> return (Just c)
    Nothing -> do
      eliminateLiteralsM
      removeTautologiesM
      m' <- getAssignment
      if m == m' then return Nothing else preprocess

-- i think this is close, slightly wrong, but close ! (maybe)
getSolutions :: CNF -> Maybe Solutions
getSolutions cnf' = do
  (solutions', _) <- RWST.evalRWST run cnf' (initState cnf')
  solutions'
  where
    run :: SolverM (Maybe Solutions)
    run = do
      preprocess >>= \case
        Just _ -> return mempty -- failure
        Nothing -> solver

    solver :: SolverM (Maybe Solutions)
    solver = do
      simplifyM
      ifM allVariablesAssigned (Just <$> solutions) branch

    branch :: SolverM (Maybe Solutions)
    branch = do
      vsids <- getVSIDS
      let lit = pickLiteral vsids
      try lit

    try :: Literal -> SolverM (Maybe Solutions)
    try c = tryAssign c True <|> tryAssign c False

    tryAssign :: Literal -> Bool -> SolverM (Maybe Solutions)
    tryAssign c v = do
      decayM
      increaseDecisionLevel
      assignM c v
      addDecision c
      propagate

    propagate :: SolverM (Maybe Solutions)
    propagate = do
      unitPropagateM >>= \case
        Just c -> handleConflict c
        Nothing -> trace "no conflict" solver

    handleConflict :: Clause -> SolverM (Maybe Solutions)
    handleConflict c = do 
      traceM "conflict"
      (clause, dl) <- analyseConflict c
      if dl < 0 then return mempty else do
        adjustScoresM clause
        learn clause
        backtrack dl
        solver

{-# INLINEABLE getSolutions #-}

-- | Checks if a CNF is satisfiable.
satisfiable :: CNF -> Bool
satisfiable cnf = case getSolutions cnf of
  Nothing -> False
  Just _ -> True
{-# INLINEABLE satisfiable #-}

bruteForce :: Expr Int -> Maybe Solutions
bruteForce expr = let cnf = toCNF expr in go cnf mempty
  where
    go :: CNF -> Solutions -> Maybe Solutions
    go cnf m = case findFreeVariable cnf of
      Nothing -> if isSat cnf then Just m else Nothing
      Just c -> try c True <|> try c False
      where
        try :: Literal -> Bool -> Maybe Solutions
        try c v = go (substitute c v cnf) (IntSet.insert c m)

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
        try c v = go (substitute c v cnf') (IntMap.insert c (v, 0) m')

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
        try c v = go (substitute c v cnf') (IntMap.insert c (v, 0) m')

        isSat :: CNF -> Bool
        isSat (CNF clauses) = null clauses

        cnf' :: CNF
        (m', cnf') = eliminateLiterals cnf m 0
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
        try c v = go (substitute c v cnf'') (IntMap.insert c (v, 0) m'')

        isSat :: CNF -> Bool
        isSat (CNF clauses) = null clauses

        cnf' :: CNF
        (m', cnf') = eliminateLiterals cnf m 0

        cnf'' :: CNF
        (m'', cnf'') = unitPropagate cnf' m' 0
{-# INLINEABLE withPureLiteralAndUnitPropagation #-}

pureLitOnlyAsPreprocess :: Expr Int -> Maybe Solutions
pureLitOnlyAsPreprocess expr = go cnf' m
  where
    cnf = toCNF expr
    (m, cnf') = eliminateLiterals cnf mempty 0

    go :: CNF -> Assignment -> Maybe Solutions
    go cnf'' m' = case findFreeVariable cnf'' of
      Nothing -> if isSat cnf'' then Just (solutionsFromAssignment m'') else Nothing
      Just c -> try c True <|> try c False
      where
        try :: Literal -> Bool -> Maybe Solutions
        try c v = go (substitute c v cnf''') (IntMap.insert c (v, 0) m'')

        isSat :: CNF -> Bool
        isSat (CNF clauses) = null clauses

        (m'', cnf''') = unitPropagate cnf'' m' 0
{-# INLINEABLE pureLitOnlyAsPreprocess #-}

literalEvery100 :: Expr Int -> Maybe Solutions
literalEvery100 expr = go cnf' m 0
  where
    cnf = toCNF expr
    (m, cnf') = eliminateLiterals cnf mempty 0

    go :: CNF -> Assignment -> Int -> Maybe Solutions
    go cnf'' m' i = case findFreeVariable cnf'''' of
      Nothing -> if isSat cnf'''' then Just (solutionsFromAssignment m''') else Nothing
      Just c -> try c True <|> try c False
      where
        try :: Literal -> Bool -> Maybe Solutions
        try c v = go (substitute c v cnf'''') (IntMap.insert c (v, 0) m''') (i + 1)

        isSat :: CNF -> Bool
        isSat (CNF clauses) = null clauses

        (m'', cnf''') = unitPropagate cnf'' m' 0
        (m''', cnf'''') = if i `mod` 100 == 0 then eliminateLiterals cnf''' m'' 0 else (m'', cnf''')
{-# INLINEABLE literalEvery100 #-}

allVariablesAssigned :: SolverM Bool
allVariablesAssigned = do
  SolverState { variables, assignment } <- get
  return $ IntSet.null $ variables `IntSet.difference` IntMap.keysSet assignment