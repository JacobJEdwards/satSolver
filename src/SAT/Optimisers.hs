{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Strict #-}

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
    pickVariable,
    pickVariableM,
    literalPolarities,
    decayM,
    eliminateLiteralsM,
    findUnitClause,
    unitPropagateM,
    assignM,
    removeTautologies,
    isSat,
    isUnsat,
    addDecision,
    addPropagation,
    isSatisfied,
    adjustScoresM,
    getClauseStatus,
    findCanditateWatchedLiteral,
    ClauseStatus (..),
    pickLiteralM,
  )
where

import Control.Monad.State.Strict (get, modify)
import Data.IntMap.Strict (type IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (type IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (type Type)
import Data.List (find)
import Data.Set (type Set)
import Data.Set qualified as Set
import SAT.CNF (type CNF (CNF), Clause, literalValue, varOfLiteral, type Assignment, type DecisionLevel, type Literal)
import SAT.DIMACS.CNF (invert)
import SAT.Monad (getAssignment, getClauseDB, getDecisionLevel, getImplicationGraph, getVSIDS, type SolverM, type SolverState (..))
import SAT.Polarity (type Polarity (Mixed, Negative, Positive))
import SAT.VSIDS (adjustScores, decay, pickLiteral, type VSIDS)

-- | Collects all literals in a CNF.
--
-- >>> collectLiterals (CNF [[1, 2], [2, 3], [3, 4]])
-- [1,2,2,3,3,4]
collectLiterals :: CNF -> [Int]
collectLiterals (CNF clauses) = concatMap getVars clauses
  where
    getVars :: Clause -> [Int]
    getVars = fmap varOfLiteral
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
literalPolarities (CNF clauses) = foldl updatePolarity mempty $ concatMap clausePolarities clauses
  where
    clausePolarities :: Clause -> [(Int, Polarity)]
    clausePolarities clause = [(varOfLiteral lit, literalPolarity lit) | lit <- clause]

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
eliminateLiterals :: CNF -> Assignment -> (Assignment, CNF)
eliminateLiterals cnf solutions = (solutions', cnf')
  where
    literals :: IntMap Polarity
    literals = IntMap.filter (/= Mixed) $ literalPolarities cnf

    cnf' :: CNF
    solutions' :: Assignment
    (cnf', solutions') = getClauses literals cnf solutions

    getClauses :: IntMap Polarity -> CNF -> Assignment -> (CNF, Assignment)
    getClauses pols expr sols = case IntMap.minViewWithKey pols of
      Nothing -> (expr, sols)
      Just ((c, p), pols') -> getClauses pols' (substitute c value expr) $ IntMap.insert c value sols
        where
          value :: Bool
          value = p == Positive
{-# INLINEABLE eliminateLiterals #-}

eliminateLiteralsM :: SolverM ()
eliminateLiteralsM = do
  SolverState {assignment} <- get
  clauses <- getClauseDB
  let partial = partialAssignment assignment $ CNF clauses
  let m = fst $ eliminateLiterals partial assignment
  modify \s -> s {assignment = m}
{-# INLINEABLE eliminateLiteralsM #-}

-- | Substitutes a literal in a CNF.
--
-- >>> substitute 1 True (CNF [[1, 2], [2, 3], [3, 4]])
-- CNF {clauses = [[2,3],[3,4]]}
substitute :: Literal -> Bool -> CNF -> CNF
substitute var val (CNF clauses) = CNF $ eliminateClause <$> filter clauseIsTrue clauses
  where
    var' :: Int
    var' = varOfLiteral var

    clauseIsTrue :: Clause -> Bool
    clauseIsTrue = not . any literalIsTrue'

    literalIsTrue' :: Literal -> Bool
    literalIsTrue' l = case l of
      v | var' == abs v && v > 0 -> val
      v | var' == abs v && v < 0 -> not val
      _ -> False

    literalIsFalse' :: Literal -> Bool
    literalIsFalse' l = case l of
      v | var' == abs v && v > 0 -> not val
      v | var' == abs v && v < 0 -> val
      _ -> False

    eliminateClause :: Clause -> Clause
    eliminateClause = filter $ not . literalIsFalse'
{-# INLINEABLE substitute #-}

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
  return (varOfLiteral p, p > 0)
  where
    unitClause :: Maybe Clause
    unitClause = find isUnitClause clauses

    isUnitClause :: Clause -> Bool
    isUnitClause = (1 ==) . length
{-# INLINEABLE findUnitClause #-}

-- | Propagates a unit clause in a CNF.
--
-- >>> unitPropagate (CNF [[1, 2], [-2, -3], [3, 4]]) IntMap.empty 0
-- (CNF {clauses = [[1,2],[-2,-3],[3,4]]},fromList [])
--
-- >>> unitPropagate (CNF [[1], [2, 3], [3, 4]]) IntMap.empty 0
-- (CNF {clauses = [[2,3],[3,4]]},fromList [(1,True)])
unitPropagate :: CNF -> Assignment -> DecisionLevel -> (Assignment, CNF)
unitPropagate cnf m dl = case findUnitClause cnf of
  Nothing -> (m, cnf)
  Just (c, p) ->
    let cnf'' = substitute c p cnf
        m' = assign m c p
     in unitPropagate cnf'' m' dl
{-# INLINEABLE unitPropagate #-}

type ClauseStatus :: Type
data ClauseStatus = Unit Literal | UNSAT | SAT | Unresolved deriving (Eq, Show)

partialAssignClause :: Assignment -> Clause -> Maybe Clause
partialAssignClause m c 
  | any (\l -> literalValue m l == Just True) c = Nothing
  | otherwise = Just $ filter (\l -> literalValue m l /= Just False) c

getClauseStatus :: Clause -> SolverM ClauseStatus
getClauseStatus clause = do
  assignment <- getAssignment
  let c = partialAssignClause assignment clause
  case c of
    Nothing -> return SAT
    Just [] -> return UNSAT
    Just [l] -> return $ Unit l
    Just _ -> return Unresolved

unitPropagateM :: SolverM (Maybe Clause)
unitPropagateM = do
  clauses <- getClauseDB

  loop clauses
  where
    loop :: [Clause] -> SolverM (Maybe Clause)
    loop clauses = process clauses False
      where
        process :: [Clause] -> Bool -> SolverM (Maybe Clause)
        process [] updated =
          if updated
            then loop clauses
            else return Nothing
        process (c : cs') updated = do
          status <- getClauseStatus c
          case status of
            SAT -> process cs' updated
            UNSAT -> return $ Just c
            Unit l -> do
              let p = l > 0
              assignM l p
              addPropagation (varOfLiteral l) c
              process cs' True
            Unresolved -> process cs' updated

findCanditateWatchedLiteral :: Clause -> SolverM (Maybe Literal)
findCanditateWatchedLiteral clause = do
  assignments <- getAssignment
  return $ find (`isUnassigned` assignments) clause

isUnassigned :: Literal -> Assignment -> Bool
isUnassigned = IntMap.notMember . varOfLiteral

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
assign :: Assignment -> Literal -> Bool -> Assignment
assign m c v = IntMap.insertWith (const id) (varOfLiteral c) v m
{-# INLINEABLE assign #-}

assignM :: Literal -> Bool -> SolverM ()
assignM c v = do
  SolverState {assignment, trail, decisionLevel} <- get
  let t = (c, decisionLevel, v) : trail
  let m = assign assignment c v
  modify \s -> s {assignment = m, trail = t}
{-# INLINEABLE assignM #-}

-- | Applies a partial assignment to a CNF.
--
-- >>> partialAssignment (IntMap.fromList [(1, (True, 0))]) (CNF [[1, 2], [-2, -3], [3, 4]])
-- CNF {clauses = [[2],[-3],[3,4]]}
partialAssignment :: Assignment -> CNF -> CNF
partialAssignment m (CNF clauses) = CNF $ filter isFalseLiteral <$> filter (not . isTrueClause) clauses
  where
    isTrueClause :: Clause -> Bool
    isTrueClause = any isTrueLiteral

    isTrueLiteral :: Literal -> Bool
    isTrueLiteral l = case literalValue m l of
      Just True -> l > 0
      Just False -> l < 0
      _ -> False

    isFalseLiteral :: Literal -> Bool
    isFalseLiteral l = case literalValue m l of
      Just False -> l >= 0
      Just True -> l <= 0
      _ -> True
{-# INLINEABLE partialAssignment #-}

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
      modify \s -> s {vsids = vs'}
      return $ pure l
    Nothing ->
      return Nothing

decayM :: SolverM ()
decayM = modify \s -> s {vsids = decay $ vsids s}
{-# INLINEABLE decayM #-}

-- | Removes tautologies from a CNF.
-- A tautology is a clause that contains both a literal and its negation, and is always true therefore.
-- This function removes all such clauses from the CNF.
--
-- >>> removeTautologies (CNF [[-2, 2], [-2, -3], [3, 4]])
-- CNF {[[-2,-3],[3,4]]}
removeTautologies :: CNF -> CNF
removeTautologies (CNF clauses) = CNF $ filter tautology clauses
  where
    tautology :: Clause -> Bool
    tautology c = not $ any (\x -> invert x `elem` c) c
{-# INLINEABLE removeTautologies #-}

-- | Checks if a CNF is satisfied
--
-- >>> isSat (CNF [[1, 2], [2, 3], [3, 4]])
-- False
--
-- >>> isSat (CNF [])
-- True
isSat :: CNF -> Bool
isSat (CNF clauses) = null clauses

-- | Checks if a CNF is unsatisfiable
--
-- >>> isUnsat (CNF [[1, 2], [2, 3], [3, 4]])
-- False
--
-- >>> isUnsat (CNF [[]])
-- True
isUnsat :: CNF -> Bool
isUnsat (CNF clauses) = any clauseIsUnsat clauses

-- | Checks if a clause is unsatisfiable
--
-- >>> clauseIsUnsat [1, 2, 3]
-- False
--
-- >>> clauseIsUnsat []
-- True
clauseIsUnsat :: Clause -> Bool
clauseIsUnsat = null

addDecision :: Literal -> SolverM ()
addDecision literal = do
  level <- getDecisionLevel
  graph <- getImplicationGraph
  let newNode = (literal, mempty, level)
  let graph' = IntMap.insert (varOfLiteral literal) newNode graph
  modify \s -> s {implicationGraph = graph'}

addPropagation :: Literal -> Clause -> SolverM ()
addPropagation literal clause = do
  level <- getDecisionLevel
  graph <- getImplicationGraph
  let newNode = (literal, pure clause, level)
  let graph' = IntMap.insert (varOfLiteral literal) newNode graph
  modify \s -> s {implicationGraph = graph'}

isSatisfied :: SolverM Bool
isSatisfied = do
  assignment <- getAssignment
  all (clauseIsSat assignment) <$> getClauseDB
  where
    clauseIsSat :: Assignment -> Clause -> Bool
    clauseIsSat m = any \l -> literalValue m l == Just True

adjustScoresM :: Clause -> SolverM ()
adjustScoresM clause = do
  vsids <- getVSIDS
  modify \s -> s {vsids = adjustScores vsids clause}

pickLiteralM :: SolverM Literal
pickLiteralM = pickLiteral <$> getVSIDS
