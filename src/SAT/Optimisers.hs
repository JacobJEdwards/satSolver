{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : SAT.Optimisers
-- Description : Exports the SAT optimisers module.
module SAT.Optimisers
  ( unitPropagate,
    substitute,
    collectLiterals,
    collectLiteralsToSet,
    uniqueOnly,
    eliminateLiterals,
    assign,
    partialAssignment,
    partialAssignmentM,
    pickVariable,
    pickVariableM,
    literalPolarities,
    decayM,
    eliminateLiteralsM,
    findUnitClause,
    unitPropagateM,
    substituteM,
    assignM,
    removeTautologies,
    removeTautologiesM,
    isNotUnsatM,
    isSat,
    isSatM,
    isUnsat,
    isUnsatM,
    backtrack,
    analyseConflict,
    addDecision,
    addPropagation,
  )
where

import Control.Monad.State.Strict (get, modify)
import Data.IntMap (type IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (type IntSet)
import Data.IntSet qualified as IntSet
import Data.List (find, partition)
import Data.Set (type Set)
import Data.Set qualified as Set
import SAT.CNF (CNF (CNF), Clause, type Assignment, type DecisionLevel, type Literal)
import SAT.Monad (SolverM, getVSIDS, getPartialAssignment, notM, getTrail, getAssignment, cnfWithLearnedClauses, getImplicationGraph, getDecisionLevel, ImplicationGraph)
import SAT.Polarity (type Polarity (Mixed, Negative, Positive))
import SAT.VSIDS (decay, type VSIDS)
import Control.Monad.RWS (MonadReader(ask))
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

-- | Collects all literals in a CNF.
--
-- >>> collectLiterals (CNF [[1, 2], [2, 3], [3, 4]])
-- [1,2,2,3,3,4]
collectLiterals :: CNF -> [Int]
collectLiterals (CNF clauses') = concatMap getVars clauses'
  where
    getVars :: Clause -> [Int]
    getVars = map abs
{-# INLINEABLE collectLiterals #-}

-- | Collects all literals in a CNF and returns them as a set.
--
-- >>> collectLiteralsToSet (CNF [[1, 2], [2, 3], [3, 4]])
-- fromList [1,2,3,4]
collectLiteralsToSet :: CNF -> IntSet
collectLiteralsToSet = IntSet.fromList . collectLiterals
{-# INLINEABLE collectLiteralsToSet #-}

-- | Finds the polarities of the literals in a CNF.
--
-- >>> literalPolarities (CNF [[1, 2], [-2, -3], [3, 4]])
-- fromList [(1,Positive),(2,Mixed),(3,Mixed),(4,Positive)]
literalPolarities :: CNF -> IntMap Polarity
literalPolarities (CNF clauses') = foldl updatePolarity IntMap.empty (concatMap clausePolarities clauses')
  where
    clausePolarities clause = [(unLit lit, literalPolarity lit) | lit <- clause]

    -- \| Unwraps a literal.
    --
    -- >>> unLit 1
    -- 1
    --
    -- >>> unLit (-1)
    -- 1
    unLit :: Literal -> Int
    unLit = abs

    -- \| Finds the polarity of a literal.
    --
    -- >>> literalPolarity 1
    -- Positive
    --
    -- >>> literalPolarity (-1)
    -- Negative
    --
    -- prop> literalPolarity x != literalPolarity (-x)
    --
    -- prop> flipPolarity (literalPolarity x) == literalPolarity (-x)
    literalPolarity :: Literal -> Polarity
    literalPolarity p
      | p < 0 = Negative
      | p > 0 = Positive
      | otherwise = Mixed

    -- \| Updates the polarities.
    -- >>> updatePolarity (IntMap.fromList [(1, Positive)]) (2, Mixed)
    -- fromList [(1,Positive),(2,Mixed)]
    updatePolarity :: IntMap Polarity -> (Int, Polarity) -> IntMap Polarity
    updatePolarity acc (lit, pol) = IntMap.insertWith (<>) lit pol acc
{-# INLINEABLE literalPolarities #-}

-- | eliminates any literals that are either positive or negative
-- from the CNF and updates the assignment
-- returns the updated CNF and assignment
eliminateLiterals :: CNF -> Assignment -> DecisionLevel -> (Assignment, CNF)
eliminateLiterals cnf solutions dl = (solutions', cnf')
  where
    literals :: IntMap Polarity
    literals = IntMap.filter (/= Mixed) $ literalPolarities cnf

    (cnf', solutions') = getClauses literals cnf solutions

    getClauses :: IntMap Polarity -> CNF -> Assignment -> (CNF, Assignment)
    getClauses pols expr sols = case IntMap.minViewWithKey pols of
      Nothing -> (expr, sols)
      Just ((c, p), pols') -> getClauses pols' (substitute c value expr) (IntMap.insert c (value, dl) sols)
        where
          value :: Bool
          value = p == Positive
{-# INLINEABLE eliminateLiterals #-} -- wrong i think

eliminateLiteralsM :: SolverM (Maybe Clause)
eliminateLiteralsM = do
  (m, _, _, _, dl, _, partial, _) <- get
  let (m', partial') = eliminateLiterals partial m dl
  modify (\(_, b, c, d, e, f, _, lc) -> (m', b, c, d, e, f, partial', lc))
  return Nothing
{-# INLINEABLE eliminateLiteralsM #-}

-- | Substitutes a literal in a CNF.
--
-- >>> substitute 1 True (CNF [[1, 2], [2, 3], [3, 4]])
-- CNF {clauses = [[2,3],[3,4]]}
substitute :: Literal -> Bool -> CNF -> CNF
substitute var val (CNF clauses) = CNF $ map (eliminateClause var val) $ filter (not . clauseIsTrue var val) clauses
  where
    clauseIsTrue :: Literal -> Bool -> Clause -> Bool
    clauseIsTrue c p = any (literalIsTrue' c p)

    literalIsTrue' :: Literal -> Bool -> Literal -> Bool
    literalIsTrue' c p l = case l of
      v | abs c == abs v && v > 0 -> p
      v | abs c == abs v && v < 0 -> not p
      _ -> False

    literalIsFalse' :: Literal -> Bool -> Literal -> Bool
    literalIsFalse' c p l = case l of
      v | abs c == abs v && v > 0 -> not p
      v | abs c == abs v && v < 0 -> p
      _ -> False

    eliminateClause :: Literal -> Bool -> Clause -> Clause
    eliminateClause c p = filter (not . literalIsFalse' c p)
{-# INLINEABLE substitute #-}

substituteM :: Literal -> Bool -> SolverM ()
substituteM c p = do
  (_, _, _, _, _, _, partial, _) <- get
  let partial' = substitute c p partial
  modify (\(a, b, c', d, e, f, _, lc) -> (a, b, c', d, e, f, partial', lc))
  return ()
{-# INLINEABLE substituteM #-}

-- | Finds a unit clause in a CNF (a clause with only one literal).
--
-- >>> findUnitClause (CNF [[1, 2], [-2, -3], [3, 4]])
-- Nothing
--
-- >>> findUnitClause (CNF [[1], [2, 3], [3, 4]])
-- Just (1,True)
findUnitClause :: CNF -> Maybe (Int, Bool)
findUnitClause (CNF clauses) = do
  clause <- unitClause
  let p = head clause
  return (abs p, p > 0)
  where
    unitClause :: Maybe Clause
    unitClause = find isUnitClause clauses

    isUnitClause :: Clause -> Bool
    isUnitClause c = length c == 1
{-# INLINEABLE findUnitClause #-}

-- | Propagates a unit clause in a CNF.
--
-- >>> unitPropagate (CNF [[1, 2], [-2, -3], [3, 4]]) IntMap.empty 0
-- (CNF {clauses = [[1,2],[-2,-3],[3,4]]},fromList [])
--
-- >>> unitPropagate (CNF [[1], [2, 3], [3, 4]]) IntMap.empty 0
-- (CNF {clauses = [[2,3],[3,4]]},fromList [(1,True)])
unitPropagate :: CNF -> Assignment -> DecisionLevel -> Either (Assignment, CNF) Clause
unitPropagate cnf m dl = case findUnitClause cnf of
  Nothing -> Left (m, cnf)
  Just (c, p) ->
    let cnf''@(CNF clauses) = substitute c p cnf
        m' = IntMap.insert c (p, dl) m
     in if any clauseIsUnsat clauses then Right [c] else
      unitPropagate cnf'' m' dl
{-# INLINEABLE unitPropagate #-}

-- | Finds a unit clause in a CNF (a clause with only one literal).
-- Returns the literal, its polarity, and the clause it belongs to.
findUnitClause' :: [Clause] -> Assignment -> Maybe (Int, Bool, Clause)
findUnitClause' clauses m =
  let checkUnit clause = case partialAssignment m (CNF [clause]) of
        CNF [[p]] -> Just (abs p, p > 0, clause)
        _ -> Nothing

  in case mapMaybe checkUnit clauses of
    [] -> Nothing
    (x:_) -> Just x

findUnitClauseM :: SolverM (Maybe (Int, Bool, Clause))
findUnitClauseM = do
  CNF clauses <- ask
  findUnitClause' clauses <$> getAssignment

unitPropagateM :: SolverM (Maybe Clause)
unitPropagateM = do
  CNF clauses <- ask
  let loop = do
        unit <- findUnitClauseM
        trace ("unit: " ++ show unit) $ case unit of
          Just (c, p, clause) -> do
            trace ("unit: " ++ show c ++ " " ++ show p ++ " " ++ show clause) $ assignM c p
            addPropagation c clause
            checkForConflict clauses >>= \case
              Just c' -> return $ Just c'
              Nothing -> loop
          Nothing ->  return Nothing

  loop

{-# INLINEABLE unitPropagateM #-}

checkForConflict :: [Clause] -> SolverM (Maybe Clause)
checkForConflict clauses = do
    assignment <- getAssignment
    return $ find (\c -> partialAssignment assignment (CNF [c]) == CNF [[]]) clauses

-- https://buffered.io/posts/a-better-nub/

-- | Removes duplicates from a list.
--
-- >>> uniqueOnly [1, 2, 3, 2, 1]
-- [1,2,3]
uniqueOnly :: forall a. (Ord a) => [a] -> [a]
uniqueOnly = go mempty
  where
    go :: Set a -> [a] -> [a]
    go _ [] = []
    go s (x : xs)
      | x `Set.member` s = go s xs
      | otherwise = x : go (Set.insert x s) xs
{-# INLINEABLE uniqueOnly #-}

-- | Assigns a value to a literal.
--
-- >>> assign IntMap.empty 1 True 0
-- fromList [(1,True)]
assign :: Assignment -> Literal -> Bool -> DecisionLevel -> Assignment
assign m c v dl = IntMap.insertWith (const id) (abs c) (v, dl) m
{-# INLINEABLE assign #-}

assignM :: Literal -> Bool -> SolverM ()
assignM c v = do
  (m, t, _, _, dl, _, _, _) <- get
  let t' = (c, dl, v) : t
  substituteM c v
  let m' = assign m c v dl
  modify (\(_, _, c', d, e, f, p, lc) -> (m', t', c', d, e, f, p, lc))
  return ()
{-# INLINEABLE assignM #-}

-- | Applies a partial assignment to a CNF.
--
-- >>> partialAssignment (IntMap.fromList [(1, (True, 0))]) (CNF [[1, 2], [-2, -3], [3, 4]])
-- CNF {clauses = [[2],[-3],[3,4]]}
partialAssignment :: Assignment -> CNF -> CNF
partialAssignment m (CNF clauses) = CNF $ map (filter isFalseLiteral) $ filter (not . isTrueClause) clauses
  where
    isTrueClause :: Clause -> Bool
    isTrueClause = any isTrueLiteral

    isTrueLiteral :: Literal -> Bool
    isTrueLiteral l = case IntMap.lookup (abs l) m of
      Just (True, _) -> l > 0
      Just (False, _) -> l < 0
      _ -> False

    isFalseLiteral :: Literal -> Bool
    isFalseLiteral l = case IntMap.lookup (abs l) m of
      Just (False, _) -> l <= 0
      Just (True, _) -> l >= 0
      _ -> True
{-# INLINEABLE partialAssignment #-}

partialAssignmentM :: SolverM ()
partialAssignmentM = do
  (m, _, _, _, _, _, partial, _) <- get
  let partial' = partialAssignment m partial
  modify (\(a, b, c, d, e, f, _, lc) -> (a, b, c, d, e, f, partial', lc))
{-# INLINEABLE partialAssignmentM #-}

-- | Picks a variable.
--
-- >>> pickVariable (IntMap.fromList [(1, 1), (2, 2), (3, 3)])
-- Just (3,fromList [(1,1.0),(2,2.0)])
pickVariable :: VSIDS -> Maybe (Literal, VSIDS)
pickVariable vs = do
  ((k, _), vsids') <- IntMap.maxViewWithKey vs
  return (k, vsids')
{-# INLINEABLE pickVariable #-}

pickVariableM :: SolverM (Maybe Literal)
pickVariableM = do
  vs <- getVSIDS
  case pickVariable vs of
    Just (l, vs') -> do
      modify (\(a, b, c, d, e, _, p, lc) -> (a, b, c, d, e, vs', p, lc))
      return $ Just l
    Nothing ->
      return Nothing

decayM :: SolverM ()
decayM = do
  vs <- getVSIDS
  let vs' = decay vs
  modify (\(a, b, c, d, e, _, p, lc) -> (a, b, c, d, e, vs', p, lc))
  return ()
{-# INLINEABLE decayM #-}


-- | Removes tautologies from a CNF.
-- A tautology is a clause that contains both a literal and its negation, and is always true therefore.
-- This function removes all such clauses from the CNF.
--
-- >>> removeTautologies (CNF [[-2, 2], [-2, -3], [3, 4]])
-- CNF {[[-2,-3],[3,4]]}
removeTautologies :: CNF -> CNF
removeTautologies (CNF clauses') = CNF $ filter (not . tautology) clauses'
  where
    tautology :: Clause -> Bool
    tautology c = any (\x -> -x `elem` c) c
{-# INLINEABLE removeTautologies #-}

removeTautologiesM :: SolverM ()
removeTautologiesM = do
  (_, _, _, _, _, _, partial, _) <- get
  let partial' = removeTautologies partial
  modify (\(a, b, c, d, e, f, _, lc) -> (a, b, c, d, e, f, partial', lc))
  return ()
{-# INLINEABLE removeTautologiesM #-}

-- | Checks if a CNF is satisfied
-- 
-- >>> isSat (CNF [[1, 2], [2, 3], [3, 4]])
-- False
-- 
-- >>> isSat (CNF [])
-- True
isSat :: CNF -> Bool
isSat (CNF clauses) = null clauses

isSatM :: SolverM Bool
isSatM = isSat <$> getPartialAssignment

-- | Checks if a CNF is unsatisfiable
-- 
-- >>> isUnsat (CNF [[1, 2], [2, 3], [3, 4]])
-- False
-- 
-- >>> isUnsat (CNF [[]])
-- True
isUnsat :: CNF -> Bool
isUnsat (CNF clauses) = any clauseIsUnsat clauses

isUnsatM :: SolverM Bool
isUnsatM = isUnsat <$> getPartialAssignment

isNotUnsatM :: SolverM Bool
isNotUnsatM = notM isUnsatM

-- | Checks if a clause is unsatisfiable
-- 
-- >>> clauseIsUnsat [1, 2, 3]
-- False
-- 
-- >>> clauseIsUnsat []
-- True
clauseIsUnsat :: Clause -> Bool
clauseIsUnsat = null

-- | Backtracks to a given decision level.
backtrack :: DecisionLevel -> SolverM ()
backtrack dl = do
    trail <- getTrail
    assignments <- getAssignment
    cnf <- cnfWithLearnedClauses
    ig <- getImplicationGraph

    let (trail', toRemove) = partition (\(_, dl', _) -> dl' < dl) trail
        toRemoveKeys = Set.fromList $ map (\(l, _, _) -> abs l) toRemove
        assignments' = IntMap.filterWithKey (\k _ -> k `notElem` toRemoveKeys) assignments
        ig' = IntMap.filterWithKey (\k _ -> k `notElem` toRemoveKeys) ig
    modify $ \(_, _, _, wl, _, vsids, _, lc) -> (assignments', trail', ig', wl, dl, vsids, cnf, lc)
    partialAssignmentM

analyseConflict :: Clause -> SolverM (Clause, DecisionLevel)
analyseConflict conflict = return (conflict, 0)





addDecision :: Literal -> SolverM ()
addDecision literal = do
    level <- getDecisionLevel
    graph <- getImplicationGraph
    let newNode = (literal, Nothing, level)
    let graph' = IntMap.insert (abs literal) newNode graph
    modify $ \ (a, b, _, d, e, f, g, h) -> (a, b, graph', d, e, f, g, h)

addPropagation :: Literal -> Clause -> SolverM ()
addPropagation literal clause = do
    level <- getDecisionLevel
    graph <- getImplicationGraph
    let newNode = (literal, Just clause, level)
    let graph' = IntMap.insert (abs literal) newNode graph
    modify $ \ (a, b, _, d, e, f, g, h) -> (a, b, graph', d, e, f, g, h)