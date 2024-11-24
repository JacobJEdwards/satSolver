{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

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
import Control.Monad.RWS.Strict qualified as RWST
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe (listToMaybe)
import SAT.CNF (toCNF, type Assignment, type CNF (CNF), type Clause, type Literal)
import SAT.Expr (Expr, type Solutions)
import SAT.Monad (SolverM, SolverState, WatchedLiterals, getAssignment, guardM, ifM, learn, increaseDecisionLevel)
import SAT.Optimisers (assignM, collectLiterals, decayM, eliminateLiterals, eliminateLiteralsM, isNotUnsatM, isSatM, pickVariableM, removeTautologiesM, substitute, unitPropagate, unitPropagateM, analyseConflict, backtrack, addDecision)
import SAT.VSIDS (initVSIDS, decay)
import Debug.Trace (trace)

-- | Initializes the watched literals.
initWatchedLiterals :: CNF -> WatchedLiterals
initWatchedLiterals (CNF clauses) = foldl updateWatchedLiterals IntMap.empty clauses
  where
    updateWatchedLiterals :: WatchedLiterals -> Clause -> WatchedLiterals
    updateWatchedLiterals wl clause =
      case take 2 $ collectLiterals $ CNF [clause] of
        [l1, l2] -> IntMap.insertWith (++) l1 [clause] $ IntMap.insertWith (++) l2 [clause] wl
        [l] -> IntMap.insertWith (++) l [clause] wl
        _ -> wl

-- | Initializes the solver state.
initState :: CNF -> SolverState
initState cnf = (mempty, mempty, mempty, initWatchedLiterals cnf, 0, initVSIDS cnf, cnf, mempty)

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
  (m, _, _, _, dl, _, partial, _) <- RWST.get
  when (mod dl 100 == 0) $ do
    let (m', partial') = eliminateLiterals partial m dl
    RWST.modify (\(_, b, c, d, e, f, _, lc) -> (m', b, c, d, e, f, partial', lc))
    return ()

simplifyM :: SolverM ()
simplifyM = do
  maybeEliminateLiterals

-- removeTautologiesM

preprocess :: SolverM (Maybe Clause)
preprocess = do
  m <- getAssignment
  unitPropagateM >>= \case
    Just c -> return (Just c)
    Nothing -> do
      eliminateLiteralsM >>= \case
        Just c -> return (Just c)
        Nothing -> do
          removeTautologiesM
          m' <- getAssignment
          if m == m' then return Nothing else preprocess

getSolutions :: CNF -> Maybe Solutions
getSolutions cnf' = do
  (solutions', _) <- RWST.evalRWST run cnf' (initState cnf')
  return solutions'
  where
    run :: SolverM Solutions
    run = do
      preprocess >>= \case
        Just _ -> return mempty -- failure
        Nothing -> solver

    solver :: SolverM Solutions
    solver = do
      -- simplifyM
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
      assignM c v
      addDecision c
      increaseDecisionLevel
      propagate

    propagate :: SolverM Solutions
    propagate = do
      trace "propagate" $ unitPropagateM >>= \case
        Just c -> trace "propagate failed" $ handleConflict c
        Nothing -> trace "propagate success" solver

    handleConflict :: Clause -> SolverM Solutions
    handleConflict c = do 
      (clause, dl) <- analyseConflict c
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
        Left(m', cnf') = unitPropagate cnf m 0
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
        Left(m'', cnf'') = unitPropagate cnf' m' 0
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

        Left(m'', cnf''') = unitPropagate cnf'' m' 0
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

        Left(m'', cnf''') = unitPropagate cnf'' m' 0
        (m''', cnf'''') = if i `mod` 100 == 0 then eliminateLiterals cnf''' m'' 0 else (m'', cnf''')
{-# INLINEABLE literalEvery100 #-}