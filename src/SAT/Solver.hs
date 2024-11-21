{-|
Module      : SAT.Solver
Description : Exports the SAT solver module.
-}

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

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
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe (listToMaybe)
import SAT.CNF (type CNF (CNF), type Clause, type Literal, type Assignment, toCNF)
import SAT.Expr (type Solutions, Expr)
import SAT.Optimisers (collectLiterals, eliminateLiterals, pickVariableM, decayM, eliminateLiteralsM, unitPropagateM, assignM, removeTautologiesM, isNotUnsatM, isSatM, substitute, unitPropagate)
import SAT.VSIDS (initVSIDS)
import Control.Monad.RWS.Strict qualified as RWST
import Control.Monad (when, unless)

import SAT.Monad (SolverM, SolverState, WatchedLiterals, getAssignment, ifM, guardM)

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
initState cnf = (mempty, mempty, mempty, initWatchedLiterals cnf, 0, initVSIDS cnf, cnf)

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
  (m, _, _, _, dl, _, partial) <- RWST.get
  when (mod dl 100 == 0) $ do
    let (m', partial') = eliminateLiterals partial m dl
    RWST.modify (\(_, b, c, d, e, f, _) -> (m', b, c, d, e, f, partial'))
    return ()


simplifyM :: SolverM ()
simplifyM = do
  maybeEliminateLiterals
  -- removeTautologiesM
  return ()

preprocess :: SolverM ()
preprocess = do
  m <- getAssignment
  _ <- unitPropagateM
  eliminateLiteralsM
  removeTautologiesM
  m' <- getAssignment
  unless (m == m') preprocess


getSolutions :: CNF -> Maybe Solutions
getSolutions cnf' = do
   (solutions', _) <- RWST.evalRWST run cnf' (initState cnf')
   return solutions'
  where
    run :: SolverM Solutions
    run = preprocess >> solver

    solver :: SolverM Solutions
    solver = do
      simplifyM
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
      propagate

    propagate :: SolverM Solutions
    propagate = do
      conflict <- unitPropagateM
      case conflict of
        Just c -> handleConflict (Just c)
        Nothing -> solver

    handleConflict :: Maybe Clause -> SolverM Solutions
    handleConflict conflict = undefined
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