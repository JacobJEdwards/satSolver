{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : SAT.Optimisers
-- Description : Exports the SAT optimisers module.
module SAT.Optimisers
  ( unitPropagate,
    literalElimination,
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
    decayM,
  )
where

import Control.Monad (guard)
import Control.Monad.RWS.Strict (RWST)
import Control.Monad.RWS.Strict qualified as RWST
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, gets, modify, put)
import Control.Monad.Writer.Strict (tell)
import Data.IntMap (type IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (type IntSet)
import Data.IntSet qualified as IntSet
import Data.List (find)
import Data.Set (type Set)
import Data.Set qualified as Set
import SAT.CNF (CNF (CNF), Clause, type Assignment, type DecisionLevel, type Literal)
import SAT.Monad (ImplicationGraph, Reason, SolverLog, SolverM, SolverState, Trail, WatchedLiterals)
import SAT.Polarity (type Polarity (Mixed, Negative, Positive))
import SAT.VSIDS (decay, type VSIDS)

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
eliminateLiterals :: CNF -> Assignment -> DecisionLevel -> (CNF, Assignment)
eliminateLiterals (CNF clauses) solutions dl = (clauses', solutions')
  where
    literals :: IntMap Polarity
    literals = IntMap.filter (/= Mixed) $ literalPolarities (CNF clauses)

    (clauses', solutions') = getClauses literals (CNF clauses) solutions

    getClauses :: IntMap Polarity -> CNF -> Assignment -> (CNF, Assignment)
    getClauses pols expr sols = case IntMap.minViewWithKey pols of
      Nothing -> (expr, sols)
      Just ((c, p), pols') -> getClauses pols' (substitute c value expr) (IntMap.insert c (value, dl) sols)
        where
          value :: Bool
          value = p == Positive
{-# INLINEABLE eliminateLiterals #-} -- wrong i think

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

-- | Eliminates any literals that are either positive or negative
-- from the CNF and updates the assignment
literalElimination :: CNF -> Assignment -> DecisionLevel -> (CNF, Assignment)
literalElimination = eliminateLiterals
{-# INLINEABLE literalElimination #-}

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
unitPropagate :: CNF -> Assignment -> DecisionLevel -> (CNF, Assignment)
unitPropagate e m dl = case findUnitClause e of
  Nothing -> (e, m)
  Just (c, p) ->
    let newExpr = substitute c p e
        newSol = IntMap.insert c (p, dl) m
     in unitPropagate newExpr newSol dl
{-# INLINEABLE unitPropagate #-}

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

partialAssignmentM :: SolverM CNF
partialAssignmentM = do
  cnf <- ask
  (m, _, _, _, _, _) <- get
  return $ partialAssignment m cnf
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
  vs <- gets (\(_, _, _, _, _, vsids) -> vsids)
  case pickVariable vs of
    Just (l, vs') -> do
      modify (\(a, b, c, d, e, _) -> (a, b, c, d, e, vs'))
      return $ Just l
    Nothing -> return Nothing

decayM :: SolverM ()
decayM = do
  vs <- gets (\(_, _, _, _, _, vsids) -> vsids)
  let vs' = decay vs
  modify (\(a, b, c, d, e, _) -> (a, b, c, d, e, vs'))
  return ()
{-# INLINEABLE decayM #-}
