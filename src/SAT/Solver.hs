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
    getSolutions,
    type Solutions,
    checkValue,
    findFreeVariable,
  )
where

import Control.Applicative ((<|>))
import Data.IntMap (type IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe (listToMaybe)
import SAT.CNF (type CNF (CNF), type Clause, type Literal, type Assignment, type DecisionLevel)
import SAT.Expr (type Solutions)
import SAT.Optimisers (collectLiterals, eliminateLiterals, substitute, unitPropagate, assign, partialAssignment, partialAssignmentM, pickVariableM, pickVariable, decayM)
import SAT.VSIDS (decay, initVSIDS, type VSIDS)
import Control.Monad.RWS.Strict (RWST)
import Control.Monad.RWS.Strict qualified as RWST 
import Control.Monad.Writer.Strict (tell)
import Control.Monad.State.Strict (modify)
import Control.Monad (guard)
import Control.Monad qualified
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import SAT.Monad (SolverM, SolverState, SolverLog, Trail, Reason, ImplicationGraph, WatchedLiterals)


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
initState cnf = (mempty, mempty, mempty, initWatchedLiterals cnf, 0, initVSIDS cnf)

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

-- | Converts an assignment to a set of solutions.
solutionsFromAssignment :: Assignment -> Solutions
solutionsFromAssignment = IntMap.keysSet . IntMap.filter fst
{-# INLINEABLE solutionsFromAssignment #-}

solutions :: SolverM Solutions
solutions = do
  (m, _, _, _, _, _) <- RWST.get
  return $ solutionsFromAssignment m

-- | Simplifies the CNF and the assignment by eliminating pure literals and unit propagating.
simplify :: CNF -> Assignment -> VSIDS -> DecisionLevel -> (Assignment, VSIDS)
simplify cnf m vsids dl = (m'', vsids) -- TODO, move vsids through
  where
     assigned :: CNF
     assigned = partialAssignment m cnf
     
     (cnf', m') = eliminateLiterals assigned m dl
     (_, m'') = unitPropagate cnf' m' dl
     
simplifyM :: SolverM ()
simplifyM = do
  (m1, _, _, _, dl, vsids) <- RWST.get
  cnf <- RWST.ask
  let (m, vsids') = simplify cnf m1 vsids dl
  RWST.put (m, [], IntMap.empty, IntMap.empty, dl, vsids')
  return ()

-- | Gets the solutions of a CNF.
getSolutions :: CNF -> Maybe Solutions
getSolutions cnf = go $ initState cnf
  where
    go :: SolverState -> Maybe Solutions
    go (m1, trail, ig, wl, dl, vsids)
      | isUnsat assigned = Nothing
      | isSat assigned = Just $ solutionsFromAssignment m
      | otherwise = case pickVariable vsids' of
          Nothing -> if isSat assigned then Just $ solutionsFromAssignment m else Nothing
          Just (c, vsids'') -> tryAssign c True wl vsids'' <|> tryAssign c False wl vsids''
      where
        (m, vsids') = simplify cnf m1 vsids dl
        
        assigned :: CNF
        assigned = partialAssignment m cnf

        tryAssign :: Literal -> Bool -> WatchedLiterals -> VSIDS -> Maybe Solutions
        tryAssign c v wl' vsids'' =
           let newM = assign m c v dl
           in go (newM, (c, dl, v) : trail, ig, wl', dl, decay vsids'')

getSolutionsMonadic :: CNF -> Maybe Solutions
getSolutionsMonadic cnf' = do
   (solutions', _) <- RWST.evalRWST solver cnf' (initState cnf')
   return solutions'
  where 
    solver :: SolverM Solutions
    solver = do 
      simplifyM
      assigned <- partialAssignmentM
      tell ["Simplify: " ++ show assigned]
      
      guard $ not $ isUnsat assigned
      
      if isSat assigned then do
        tell ["Sat"]
        solutions
      else do
        var <- pickVariableM
        case var of
          Nothing -> do
            tell ["No variable to pick"]
            guard $ isSat assigned
            solutions
          Just c -> do
            tell ["Picked variable: " ++ show c]
            tryAssign c True <|> tryAssign c False
            
    tryAssign :: Literal -> Bool -> SolverM Solutions
    tryAssign c v = do
      decayM
      assignM c v
      tell ["Assigned " ++ show c ++ " to " ++ show v]
      solver
    
    assignM :: Literal -> Bool -> SolverM ()
    assignM c v = do
      modify $ \(m, trail, ig, wl, dl, vsids) -> (assign m c v dl, (c, dl, v) : trail, ig, wl, dl, vsids)
      tell ["Assigned " ++ show c ++ " to " ++ show v]
    
        
-- | Checks if a CNF is satisfiable.
satisfiable :: CNF -> Bool
satisfiable cnf@(CNF clauses) = case findFreeVariable cnf' of
  Nothing -> null clauses
  Just c ->
    let trueGuess = substitute c True cnf'
        falseGuess = substitute c False cnf'
     in satisfiable trueGuess || satisfiable falseGuess
  where
    cnf' = cnf
{-# INLINEABLE satisfiable #-}
