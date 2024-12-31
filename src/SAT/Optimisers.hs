{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Strict #-}
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
    pickVariableM,
    literalPolarities,
    decayM,
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
import Data.List ( find, findIndex )
import Data.Set (type Set)
import Data.Set qualified as Set
import SAT.CNF (Clause (Clause, literals, watched), literalValue, varOfLiteral, type Assignment, type CNF (CNF), type DecisionLevel, type Literal)
import SAT.DIMACS.CNF (invert)
import SAT.Monad (getAssignment, getClauseDB, getDecisionLevel, getImplicationGraph, getVSIDS, type SolverM, type SolverState (..), getWatchedLiterals, WatchedLiterals (WatchedLiterals, literals), getPropagationStack, Reason)
import SAT.Polarity (type Polarity (Mixed, Negative, Positive))
import SAT.VSIDS (adjustScores, decay, pickLiteral, pickVariable, VSIDS (VSIDS))
import Stack qualified
import Control.Monad (foldM)


-- | Collects all literals in a CNF.
--
-- >>> collectLiterals (CNF [[1, 2], [2, 3], [3, 4]])
-- [1,2,2,3,3,4]
collectLiterals :: CNF -> [Int]
collectLiterals (CNF clauses) = concatMap getVars clauses
  where
    getVars :: Clause -> [Int]
    getVars = fmap varOfLiteral . SAT.CNF.literals
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
    clausePolarities (Clause {SAT.CNF.literals}) = [(varOfLiteral lit, literalPolarity lit) | lit <- literals]

    -- | Finds the polarity of a literal.
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

    -- | Updates the polarities.
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

-- eliminateLiteralsM :: SolverM ()
-- eliminateLiteralsM = do
--   SolverState {assignment} <- get
--   clauses <- getClauseDB
--   let partial = partialAssignment assignment $ CNF clauses
--   let m = fst $ eliminateLiterals partial assignment
--   modify \s -> s {assignment = m}
-- {-# INLINEABLE eliminateLiteralsM #-}

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
    clauseIsTrue (Clause {SAT.CNF.literals}) = not . any literalIsTrue' $ literals

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
    eliminateClause Clause {SAT.CNF.literals, watched} =
      let lits = filter (not . literalIsFalse') literals
       in Clause {SAT.CNF.literals = lits, watched = watched}
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
  Clause {SAT.CNF.literals} <- unitClause
  let p = head literals
  return (varOfLiteral p, p > 0)
  where
    unitClause :: Maybe Clause
    unitClause = find isUnitClause clauses

    isUnitClause :: Clause -> Bool
    isUnitClause Clause {SAT.CNF.literals} = (1 ==) . length $ literals
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
partialAssignClause m (Clause {SAT.CNF.literals, watched})
  | any (\l -> literalValue m l == Just True) literals = Nothing
  | otherwise = Just $ Clause {SAT.CNF.literals = filter (\l -> literalValue m l /= Just False) literals, watched}

getClauseStatus :: Clause -> SolverM ClauseStatus
getClauseStatus clause = do
  assignment <- getAssignment
  let c = partialAssignClause assignment clause
  case c of
    Nothing -> return SAT
    Just (Clause {SAT.CNF.literals = []}) -> return UNSAT
    Just (Clause {SAT.CNF.literals = [l]}) -> return $ Unit l
    Just _ -> return Unresolved

unitPropagateM :: SolverM (Maybe Clause)
unitPropagateM = loop
  where
    loop :: SolverM (Maybe Clause)
    loop = do
      propagationStack <- getPropagationStack
      process propagationStack
      where
        process :: [(Literal, Bool, Maybe Reason)] -> SolverM (Maybe Clause)
        process [] = return Nothing
        process ((l, val, r) : ls) = do
          assignM l val
          addPropagation l r
          modify \s -> s {propagationStack = ls}
          wl <- getWatchedLiterals
          let clauses = IntMap.findWithDefault [] l $ SAT.Monad.literals wl
          conflict <- foldM processClause Nothing clauses
          case conflict of
            Just _ -> return conflict
            Nothing -> loop

        processClause :: Maybe Clause -> Clause -> SolverM (Maybe Clause) -- (newWatch, maybe conflict clause)
        processClause conflict clause@(Clause {watched=(a, b), SAT.CNF.literals}) = do
          case conflict of
            Just _ -> return conflict
            Nothing -> do
              assignment <- getAssignment
              let first = literals !! a
              let second = literals !! b

              let firstValue = literalValue assignment first
              let secondValue = literalValue assignment second

              case (firstValue, secondValue) of
                (Just True, _) -> return Nothing
                (_, Just True) -> return Nothing
                (Just False, Just False) -> do
                  return $ Just clause -- UNSAT
                (Nothing, Just False) -> do
                  let newWatch = findIndex (\l -> l /= first && l /= second && literalValue assignment l /= Just False) literals

                  case newWatch of
                    Just i -> do
                      let newClause = clause {watched = (a, i)}
                      let l = literals !! i

                      -- clauseDb <- getClauseDB
                      -- let !clauseDB' = filter (/= clause) clauseDb
                      -- modify \s -> s {clauseDB = newClause : clauseDB'}

                      wl <- getWatchedLiterals

                      let !aClauses = IntMap.findWithDefault [] (varOfLiteral first) $ SAT.Monad.literals wl
                      let !aClauses' = newClause : filter (/= clause) aClauses

                      let !bClauses = IntMap.findWithDefault [] (varOfLiteral second) $ SAT.Monad.literals wl
                      let !bClauses' = filter (/= clause) bClauses

                      let !lClauses = IntMap.findWithDefault [] (varOfLiteral l) $ SAT.Monad.literals wl
                      let !lClauses' = newClause : lClauses

                      let newWl = IntMap.insert (varOfLiteral first) aClauses' $ IntMap.insert (varOfLiteral second) bClauses' $ IntMap.insert (varOfLiteral l) lClauses' $ SAT.Monad.literals wl

                      modify \s -> s {watchedLiterals = WatchedLiterals newWl}

                      return Nothing
                    Nothing -> do
                      let l = literals !! a
                      modify \s -> s {propagationStack = (varOfLiteral l, l > 0, Just clause) : propagationStack s}
                      return Nothing
                (Just False, Nothing) -> do
                  let newWatch = findIndex (\l -> l /= first && l /= second && literalValue assignment l /= Just False) literals
                  case newWatch of
                    Just i -> do
                      let newClause = clause {watched = (i, b)}
                      let l = literals !! i

                      -- clauseDb <- getClauseDB
                      -- let !clauseDB' = filter (/= clause) clauseDb
                      -- modify \s -> s {clauseDB = newClause : clauseDB'}

                      wl <- getWatchedLiterals
                      let aClauses = IntMap.findWithDefault [] (varOfLiteral first) $ SAT.Monad.literals wl
                      let aClauses' = filter (/= clause) aClauses

                      let bClauses = IntMap.findWithDefault [] (varOfLiteral second) $ SAT.Monad.literals wl
                      let bClauses' = newClause : filter (/= clause) bClauses

                      let lClauses = IntMap.findWithDefault [] (varOfLiteral l) $ SAT.Monad.literals wl
                      let lClauses' = newClause : lClauses

                      let newWl = IntMap.insert (varOfLiteral first) aClauses' $ IntMap.insert (varOfLiteral second) bClauses' $ IntMap.insert (varOfLiteral l) lClauses' $ SAT.Monad.literals wl
                      modify \s -> s {watchedLiterals = WatchedLiterals newWl}
                      return Nothing
                    Nothing -> do
                      let l = literals !! b
                      modify \s -> s {propagationStack = (varOfLiteral l, l > 0, Just clause) : propagationStack s}
                      return Nothing
                (Nothing, Nothing) -> do
                  return Nothing


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
  let t = Stack.push (c, decisionLevel, v) trail
  let m = assign assignment c v
  modify \s -> s {assignment = m, trail = t}
{-# INLINEABLE assignM #-}

-- | Applies a partial assignment to a CNF.
--

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
    tautology Clause {SAT.CNF.literals} = not $ any (\x -> invert x `elem` literals) literals
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
clauseIsUnsat = null . SAT.CNF.literals

addDecision :: Literal -> SolverM ()
addDecision literal = do
  modify \s -> s {propagationStack = (abs literal, literal > 0, Nothing) : propagationStack s}

addPropagation :: Literal -> Maybe Clause -> SolverM ()
addPropagation literal clause = do
  level <- getDecisionLevel
  graph <- getImplicationGraph
  let newNode = (literal, clause, level)
  let graph' = IntMap.insert (varOfLiteral literal) newNode graph
  modify \s -> s {implicationGraph = graph'}

isSatisfied :: SolverM Bool
isSatisfied = do
  assignment <- getAssignment
  all (clauseIsSat assignment) <$> getClauseDB
  where
    clauseIsSat :: Assignment -> Clause -> Bool
    clauseIsSat m (Clause {SAT.CNF.literals}) = any (\l -> literalValue m l == Just True) literals

adjustScoresM :: [Literal] -> SolverM ()
adjustScoresM clause = do
  vsids <- getVSIDS
  modify \s -> s {vsids = adjustScores vsids clause}

pickLiteralM :: SolverM Literal
pickLiteralM = do 
  assignment <- getAssignment
  VSIDS vs <- getVSIDS

  let vs' = IntMap.filterWithKey (\k _ -> literalValue assignment k == Nothing) vs

  return $ pickLiteral (VSIDS vs')
