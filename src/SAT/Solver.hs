{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      : SAT.Solver
-- Description : Exports the SAT solver module.
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
import Control.Monad.RWS (MonadState (get), modify)
import Control.Monad.RWS.Strict qualified as RWST
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe (listToMaybe)
import SAT.CNF (toCNF, type Assignment, type CNF (CNF), type Clause, type Literal)
import SAT.Expr (Expr, type Solutions)
import SAT.Monad (ClauseState (..), SolverM, SolverState (..), WatchedLiterals, getAssignment, guardM, ifM, increaseDecisionLevel, learn)
import SAT.Optimisers (addDecision, analyseConflict, assignM, backtrack, collectLiterals, decayM, eliminateLiterals, eliminateLiteralsM, isNotUnsatM, isSatM, pickVariableM, removeTautologiesM, substitute, unitPropagate, unitPropagateM)
import SAT.VSIDS (initVSIDS)
import Debug.Trace (trace)

-- | Initializes the watched literals.
initWatchedLiterals :: CNF -> SAT.Monad.WatchedLiterals
initWatchedLiterals (CNF cs) = foldl updateWatchedLiterals IntMap.empty cs
  where
    initClauseState :: [Int] -> Clause -> SAT.Monad.ClauseState
    initClauseState literals clause = SAT.Monad.ClauseState {original = clause, current = Just clause, watched = literals}

    updateWatchedLiterals :: SAT.Monad.WatchedLiterals -> Clause -> SAT.Monad.WatchedLiterals
    updateWatchedLiterals wl clause =
      case take 2 $ collectLiterals $ CNF [clause] of
        [l1, l2] ->
          let state = initClauseState [l1, l2] clause
           in IntMap.insertWith (++) l1 [state] $ IntMap.insertWith (++) l2 [state] wl
        [l] -> IntMap.insertWith (++) l [initClauseState [l] clause] wl
        _ -> wl
{-# INLINEABLE initWatchedLiterals #-}

-- | Initializes the solver state.
initState :: CNF -> SAT.Monad.SolverState
initState cnf =
  SAT.Monad.SolverState
    { assignment = mempty,
      trail = mempty,
      implicationGraph = mempty,
      watchedLiterals = initWatchedLiterals cnf,
      decisionLevel = 0,
      vsids = initVSIDS cnf,
      clauseDB = [],
      partial = cnf,
      learnedClauses = mempty
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
solutionsFromAssignment = IntMap.keysSet
{-# INLINEABLE solutionsFromAssignment #-}

solutions :: SAT.Monad.SolverM Solutions
solutions = solutionsFromAssignment <$> SAT.Monad.getAssignment

maybeEliminateLiterals :: SAT.Monad.SolverM ()
maybeEliminateLiterals = do
  SAT.Monad.SolverState {partial, decisionLevel, assignment} <- get
  when (mod decisionLevel 100 == 0) $ do
    let (m, partial') = eliminateLiterals partial assignment
    modify $ \s -> s {assignment = m, partial = partial'}
    return ()

simplifyM :: SAT.Monad.SolverM ()
simplifyM = do
  maybeEliminateLiterals

-- removeTautologiesM

preprocess :: SAT.Monad.SolverM Bool
preprocess = do
  m <- SAT.Monad.getAssignment
  trace "Eliminating literals" $ eliminateLiteralsM
  unitPropagateM >>= \case
    Just _ -> return True
    Nothing -> do
      eliminateLiteralsM 
      m' <- SAT.Monad.getAssignment
      if m == m' then return False else preprocess

getSolutions :: CNF -> Maybe Solutions
getSolutions cnf' = do
  (solutions', _) <- RWST.evalRWST run cnf' (initState cnf')
  return solutions'
  where
    run :: SAT.Monad.SolverM Solutions
    run = do
      trace "Preprocessing" $ preprocess >>= \case
        True -> return mempty -- failure
        False -> solver

    solver :: SAT.Monad.SolverM Solutions
    solver = do
      trace "Solving" $ simplifyM
      guardM isNotUnsatM
      ifM isSatM solutions branch

    branch :: SolverM Solutions
    branch = maybe failed try =<< pickVariableM

    failed :: SolverM Solutions
    failed = return mempty

    try :: Literal -> SolverM Solutions
    try c = tryAssign c True <|> tryAssign c False

    tryAssign :: Literal -> Bool -> SolverM Solutions
    tryAssign c v = do
      decayM
      trace ("Trying " ++ show c ++ " as " ++ show v) $ assignM c v
      -- _ <- assignM c v
      _ <- trace ("Assigned " ++ show c ++ " to " ++ show v) $ addDecision c
      increaseDecisionLevel
      propagate

    propagate :: SolverM Solutions
    propagate = do
      unitPropagateM >>= \case
        Just c -> solver
        Nothing -> solver

    handleConflict :: Clause -> SolverM Solutions
    handleConflict c = do
      (clause, dl) <- analyseConflict c
      SAT.Monad.learn clause
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
        try c v = go (substitute c v cnf') (IntMap.insert c v m')

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
        try c v = go (substitute c v cnf') (IntMap.insert c v m')

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
        try c v = go (substitute c v cnf'') (IntMap.insert c v m'')

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
        try c v = go (substitute c v cnf''') (IntMap.insert c v m'')

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
        try c v = go (substitute c v cnf'''') (IntMap.insert c v m''') (i + 1)

        isSat :: CNF -> Bool
        isSat (CNF clauses) = null clauses

        (m'', cnf''') = unitPropagate cnf'' m' 0
        (m''', cnf'''') = if i `mod` 100 == 0 then eliminateLiterals cnf''' m'' else (m'', cnf''')
{-# INLINEABLE literalEvery100 #-}