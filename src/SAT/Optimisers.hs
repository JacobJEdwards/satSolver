{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

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

import Control.Monad.State.Strict (modify, MonadState (get))
import Data.IntMap (type IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (type IntSet)
import Data.IntSet qualified as IntSet
import Data.List (find, partition)
import Data.Set (type Set)
import Data.Set qualified as Set
import SAT.CNF (CNF (CNF), Clause, type Assignment, type DecisionLevel, type Literal)
import SAT.Monad (type SolverM, getVSIDS, getPartialAssignment, notM, getAssignment, cnfWithLearnedClauses, type SolverState(assignment, partial, vsids, implicationGraph, SolverState, decisionLevel, watchedLiterals), implicationGraph, trail, ClauseState (..), getWatchedLiterals, WatchedLiterals)
import SAT.Polarity (type Polarity (Mixed, Negative, Positive))
import SAT.VSIDS (decay, type VSIDS)
import Control.Monad.RWS ( gets )
import Data.Maybe (mapMaybe, catMaybes, isNothing)
import Control.Monad (foldM)
import Debug.Trace (trace, traceM)

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
eliminateLiterals :: CNF -> Assignment -> (Assignment, CNF)
eliminateLiterals cnf solutions = (solutions', cnf')
  where
    literals :: IntMap Polarity
    literals = IntMap.filter (/= Mixed) $ literalPolarities cnf

    (cnf', solutions') = getClauses literals cnf solutions

    getClauses :: IntMap Polarity -> CNF -> Assignment -> (CNF, Assignment)
    getClauses pols expr sols = case IntMap.minViewWithKey pols of
      Nothing -> (expr, sols)
      Just ((c, p), pols') -> getClauses pols' (substitute c value expr) (IntMap.insert c value sols)
        where
          value :: Bool
          value = p == Positive
{-# INLINEABLE eliminateLiterals #-} -- wrong i think

eliminateLiteralsM :: SolverM ()
eliminateLiteralsM = do
  SolverState { partial, assignment} <- get
  let (m, partial') = eliminateLiterals partial assignment
  modify $ \s -> s { assignment = m, partial = partial'}
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
  modify $ \s -> s { partial = substitute c p $ partial s }
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
unitPropagate :: CNF -> Assignment -> DecisionLevel -> (Assignment, CNF)
unitPropagate cnf m dl = case findUnitClause cnf of
  Nothing -> (m, cnf)
  Just (c, p) ->
    let cnf'' = substitute c p cnf
        m' = IntMap.insert c p m
    in unitPropagate cnf'' m' dl
{-# INLINEABLE unitPropagate #-}


allClauseStates :: SolverM [ClauseState]
allClauseStates = do
  SolverState { watchedLiterals } <- get
  return $ concat $ IntMap.elems watchedLiterals

-- | Finds a unit clause in a CNF (a clause with only one literal).
-- Returns the literal, its polarity, and the clause it belongs to.
findUnitClause' :: [ClauseState] -> Maybe (Int, Bool)
findUnitClause' cs =
  let unitClause = find isUnitClause cs
  in case unitClause of
    Just ClauseState { watched = [l] } -> Just (abs l, l > 0)
    _ -> Nothing
  where
    isUnitClause :: ClauseState -> Bool
    isUnitClause ClauseState { watched = [_] } = True
    isUnitClause _ = False

{-# INLINEABLE findUnitClause' #-}

findUnitClauseM :: SolverM (Maybe (Int, Bool))
findUnitClauseM = do
  findUnitClause' <$> allClauseStates
{-# INLINEABLE findUnitClauseM #-}

unitPropagateM :: SolverM (Maybe Clause)
unitPropagateM = do
  let loop = do
        unit <- findUnitClauseM
        case unit of
          Just (c, p) -> do
            traceM $ "Unit clause: " ++ show c ++ " " ++ show p
            conflict <- assignM c p
            case conflict of
              Just c' -> return $ Just c'
              Nothing ->
                loop
          Nothing ->  return Nothing
  loop

{-# INLINEABLE unitPropagateM #-}

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
assign m c v = IntMap.insertWith (const id) (abs c) v m
{-# INLINEABLE assign #-}

newWatched :: Literal -> ClauseState -> SolverM ()
newWatched l cs = do
  SolverState { watchedLiterals } <- get
  let watched = IntMap.findWithDefault [] l watchedLiterals
  let watchedLiterals' = IntMap.insert l (cs : watched) watchedLiterals
  modify $ \s -> s { watchedLiterals = watchedLiterals' }
{-# INLINEABLE newWatched #-}

-- | Updates the clause state.
updateClauseState :: ClauseState -> Literal -> SolverM ClauseState
updateClauseState cs@ClauseState { current, watched, original } l = do
  assignments <- getAssignment
  let watched' = filter (/= l) watched
  -- find new watched literals

  -- current is maybe clause

  -- not quite right, shouldnt do all the checks i think
  let newW = case original of
        [] -> []
        clause -> -- find first literal that is not assigned
          let l' = find (\l -> isNothing (IntMap.lookup (abs l) assignments)) clause
          in case l' of
            Just l'' -> [l'']
            Nothing -> []

  traceM $ "New watched: " ++ show newW

  let current' = case current of
        Just [] -> Just []
        Just clause -> Just $ filter (\l -> IntMap.lookup (abs l) assignments == Just (l > 0)) clause
        Nothing -> Nothing

  traceM $ "New current: " ++ show current'

  let new = cs { current = current', watched = newW ++ watched' }
  mapM_ (`newWatched` new) newW

  return new


checkWatched :: Literal -> Bool -> SolverM (Maybe Clause) -- conflict clause
checkWatched c v = do
  SolverState { watchedLiterals } <- get
  let watched = IntMap.findWithDefault [] c watchedLiterals -- wathced is clause states
  let watchedLiterals' = IntMap.delete c watchedLiterals
  modify $ \s -> s { watchedLiterals = watchedLiterals' }
  newStates <- mapM (`updateClauseState` c) watched

  let conflicts = mapM (\ClauseState{ original, current} -> do
        case current of
          Just [] -> Just original
          _ ->
            Nothing
        ) newStates

  return $ conflicts >>= \case
    [] -> Nothing
    (cl:_) -> Just cl

{-# INLINEABLE checkWatched #-}

assignM :: Literal -> Bool -> SAT.Monad.SolverM (Maybe Clause)
assignM c v = do
  SolverState { assignment, trail, decisionLevel } <- get
  let t = (c, decisionLevel, v) : trail
  let m = assign assignment c v
  modify $ \s -> s { assignment = m, trail = t}
  checkWatched c v
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

-- looking stuff up twice not good
    isTrueLiteral :: Literal -> Bool
    isTrueLiteral l = case IntMap.lookup (abs l) m of
      Just True -> l > 0
      Just False -> l < 0
      _ -> False

    isFalseLiteral :: Literal -> Bool
    isFalseLiteral l = case IntMap.lookup (abs l) m of
      Just False -> l <= 0
      Just True -> l >= 0
      _ -> True
{-# INLINEABLE partialAssignment #-}

partialAssignmentM :: SAT.Monad.SolverM ()
partialAssignmentM = do
  SolverState { assignment, partial } <- get
  let partial' = partialAssignment assignment partial
  modify $ \s -> s { partial = partial' }
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
      modify $ \s -> s { vsids = vs'}
      return $ Just l
    Nothing ->
      return Nothing

-- | Decays the vsids contained within the Solver Monad
decayM :: SolverM ()
decayM = do
  modify $ \s -> s { vsids = decay $ vsids s }
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

removeTautologiesM :: SAT.Monad.SolverM ()
removeTautologiesM = do
  modify $ \s -> s { partial = removeTautologies $ partial s }
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

isSatM :: SAT.Monad.SolverM Bool
isSatM = isSat <$> SAT.Monad.getPartialAssignment

-- | Checks if a CNF is unsatisfiable
-- 
-- >>> isUnsat (CNF [[1, 2], [2, 3], [3, 4]])
-- False
-- 
-- >>> isUnsat (CNF [[]])
-- True
isUnsat :: CNF -> Bool
isUnsat (CNF clauses) = any clauseIsUnsat clauses

isUnsatM :: SAT.Monad.SolverM Bool
isUnsatM = isUnsat <$> SAT.Monad.getPartialAssignment

isNotUnsatM :: SAT.Monad.SolverM Bool
isNotUnsatM = SAT.Monad.notM isUnsatM

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
backtrack :: DecisionLevel -> SAT.Monad.SolverM ()
backtrack dl = do
    SolverState { trail, assignment, implicationGraph} <- get
    cnf <- cnfWithLearnedClauses

    let (trail', toRemove) = partition (\(_, dl', _) -> dl' < dl) trail
        toRemoveKeys = Set.fromList $ map (\(l, _, _) -> abs l) toRemove
        m = IntMap.filterWithKey (\k _ -> k `notElem` toRemoveKeys) assignment
        ig = IntMap.filterWithKey (\k _ -> k `notElem` toRemoveKeys) implicationGraph

    modify $ \s -> s { assignment = m, trail = trail', implicationGraph = ig, partial = cnf }
    partialAssignmentM

-- analyseConflict :: Clause -> SAT.Monad.SolverM (Clause, DecisionLevel)
-- analyseConflict conflictClause = do
--     -- Get the current solver state
--     SolverState { trail, implicationGraph, decisionLevel = currentLevel } <- get

--     -- Initialize traversing the graph
--     let initialConflictLiterals = conflictClause
--     let conflictLevelLiterals = filter (\l -> decisionLevelOf implicationGraph l == currentLevel) initialConflictLiterals
--     let visited = Set.empty

--     -- Perform resolution until we find the first UIP
--     (learnedClause, seenLevels) <- resolve conflictLevelLiterals initialConflictLiterals visited

--     -- Compute the backtrack level (max level excluding current)
--     let backtrackLevel = case Set.toList (Set.delete currentLevel seenLevels) of
--                            [] -> 0  -- Backtrack to the start
--                            xs -> maximum xs

--     return (learnedClause, backtrackLevel)

--   where
--     -- Resolve literals to derive the learned clause
--     resolve :: [Literal] -> [Literal] -> Set Literal -> SAT.Monad.SolverM (Clause, Set DecisionLevel)
--     resolve [] acc _ = do
--       SolverState { implicationGraph } <- get
--       return (acc, decisionLevelsOf implicationGraph acc)
--     resolve (l:ls) acc seen
--       | Set.member l seen = resolve ls acc seen
--       | otherwise = do
--           SolverState { implicationGraph } <- get
--           let clause = reasonFor l implicationGraph
--           let newLiterals = clause
--           resolve (ls ++ filter (`notElem` acc) newLiterals)
--                   (filter (/= l) acc ++ newLiterals)
--                   (Set.insert l seen)

--     -- Helper to extract the decision level of a literal
--     decisionLevelOf :: ImplicationGraph -> Literal -> DecisionLevel
--     decisionLevelOf g l = case IntMap.lookup (abs l) g of
--                           Just (_, _, level) -> level
--                           Nothing -> 0

--     -- Helper to extract the reason clause for a literal
--     reasonFor :: Literal -> ImplicationGraph -> Clause
--     reasonFor l graph = case IntMap.lookup (abs l) graph of
--                           Just (_, Just reason, _) -> reason
--                           _ -> []  -- No reason for a decision literal

--     -- Helper to compute decision levels of all literals in a clause
--     decisionLevelsOf :: ImplicationGraph -> [Literal] -> Set DecisionLevel
--     decisionLevelsOf g = Set.fromList . map (decisionLevelOf g)

analyseConflict :: Clause -> SolverM (Clause, DecisionLevel)
analyseConflict conflict = do
    SolverState { implicationGraph = ig, decisionLevel = currentLevel } <- get

    -- Track visited literals and clauses
    let go (seen, cutLiterals, decisionLevelLiterals) = foldr (\literal (s, c, d) ->
                if literal `Set.member` s
                then (s, c, d)  -- Skip if already visited
                else case IntMap.lookup (abs literal) ig of
                    Nothing -> (Set.insert literal s, literal : c, d)  -- Decision literal
                    Just (_, reason, level) ->
                        if level == currentLevel
                        then (Set.insert literal s, c, literal : d)  -- Current level
                        else (Set.insert literal s, literal : c, d)  -- Earlier level
            ) (seen, cutLiterals, decisionLevelLiterals)

    let (seenLiterals, cutLiterals, currentLevelLiterals) = foldl go (Set.empty, [], []) [conflict]

    -- Ensure there is exactly one UIP at the current level
    case currentLevelLiterals of
        [uip] -> do
            let learnedClause = cutLiterals ++ [negate uip]
            let backtrackLevel = maximum $ 0 : [dl | literal <- cutLiterals, let Just (_, _, dl) = IntMap.lookup (abs literal) ig, dl < currentLevel]
            return (learnedClause, backtrackLevel)
        _ -> error "analyseConflict: No UIP found or multiple UIPs"



addDecision :: Literal -> SAT.Monad.SolverM ()
addDecision literal = do
    SolverState { implicationGraph = graph, decisionLevel = level } <- get
    let newNode = (literal, Nothing, level)
    let graph' = IntMap.insert (abs literal) newNode graph
    modify $ \s -> s { implicationGraph = graph' }

addPropagation :: Literal -> Clause -> SolverM ()
addPropagation literal clause = do
    SolverState { implicationGraph = graph, decisionLevel = level } <- get
    let newNode = (literal, Just clause, level)
    let graph' = IntMap.insert (abs literal) newNode graph
    modify $ \s -> s { implicationGraph = graph' }