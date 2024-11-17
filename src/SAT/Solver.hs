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
import SAT.Optimisers (collectLiterals, eliminateLiterals, substitute, unitPropagate, assign, partialAssignment)
import SAT.VSIDS (decay, initVSIDS, pickVariable, type VSIDS)


type Trail = [(Literal, DecisionLevel, Bool)]
type Reason = Clause
type ImplicationGraph = IntMap (Reason, DecisionLevel)
type WatchedLiterals = IntMap [Clause]

type SolverState = (Assignment, Trail, ImplicationGraph, WatchedLiterals, DecisionLevel, VSIDS)

initWatchedLiterals :: CNF -> WatchedLiterals
initWatchedLiterals (CNF clauses) = foldl updateWatchedLiterals IntMap.empty clauses
  where
    updateWatchedLiterals :: WatchedLiterals -> Clause -> WatchedLiterals
    updateWatchedLiterals wl clause =
      case take 2 $ collectLiterals $ CNF [clause] of
        [l1, l2] -> IntMap.insertWith (++) l1 [clause] $ IntMap.insertWith (++) l2 [clause] wl
        [l] -> IntMap.insertWith (++) l [clause] wl
        _ -> wl

initState :: CNF -> SolverState
initState cnf = (mempty, mempty, mempty, initWatchedLiterals cnf, 0, initVSIDS cnf)


findFreeVariable :: CNF -> Maybe Literal
findFreeVariable = listToMaybe . collectLiterals
{-# INLINEABLE findFreeVariable #-}

checkValue :: Solutions -> Literal -> Bool
checkValue = flip IntSet.member
{-# INLINEABLE checkValue #-}

isSat :: CNF -> Bool
isSat (CNF clauses) = null clauses

isUnsat :: CNF -> Bool
isUnsat (CNF clauses) = any clauseIsUnsat clauses

clauseIsUnsat :: Clause -> Bool
clauseIsUnsat = null


solutionsFromAssignment :: Assignment -> Solutions
solutionsFromAssignment = IntMap.keysSet . IntMap.filter fst
{-# INLINEABLE solutionsFromAssignment #-}

simplify :: CNF -> Assignment -> VSIDS -> DecisionLevel -> (Assignment, VSIDS)
simplify cnf m vsids dl = (m'', vsids) -- TODO, move vsids through
  where
     assigned :: CNF
     assigned = partialAssignment m cnf
     
     (cnf', m') = eliminateLiterals assigned m dl
     (_, m'') = unitPropagate cnf' m' dl

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
