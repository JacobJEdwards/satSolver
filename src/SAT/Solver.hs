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
import Data.List (foldl')
import Data.Maybe (listToMaybe)
import SAT.CNF (type CNF (CNF), type Clause, type Literal, addClause)
import SAT.Expr (type Solutions)
import SAT.Optimisers (collectLiterals, eliminateLiterals, substitute, unitPropagate)

type WatchedLiterals = IntMap [Clause]

type Level = Int

type VSIDS = IntMap Double

decayFactor :: Double
decayFactor = 1.0

decay :: VSIDS -> VSIDS
decay = IntMap.map (* decayFactor)

initVSIDS :: CNF -> VSIDS
initVSIDS (CNF clauses) = foldl' updateVSIDS IntMap.empty clauses
  where
    updateVSIDS :: VSIDS -> Clause -> VSIDS
    updateVSIDS = foldl' (\vsids' l -> IntMap.insertWith (+) (abs l) 1 vsids')

pickVariable :: VSIDS -> Maybe (Int, VSIDS)
pickVariable vs = case IntMap.maxViewWithKey vs of
  Just ((k, _), vsids') -> Just (k, vsids')
  Nothing -> Nothing
{-# INLINEABLE pickVariable #-}

type Assignment = IntMap Bool

initWatchedLiterals :: CNF -> WatchedLiterals
initWatchedLiterals (CNF clauses) = foldl updateWatchedLiterals IntMap.empty clauses
  where
    updateWatchedLiterals :: WatchedLiterals -> Clause -> WatchedLiterals
    updateWatchedLiterals wl clause =
      case take 2 (collectLiterals (CNF [clause])) of
        [l1, l2] -> IntMap.insertWith (++) l1 [clause] $ IntMap.insertWith (++) l2 [clause] wl
        [l] -> IntMap.insertWith (++) l [clause] wl
        _ -> wl

findFreeVariable :: CNF -> Maybe Int
findFreeVariable = listToMaybe . collectLiterals
{-# INLINEABLE findFreeVariable #-}

checkValue :: Solutions -> Int -> Bool
checkValue = flip IntSet.member
{-# INLINEABLE checkValue #-}

isSat :: CNF -> Bool
isSat (CNF clauses) = null clauses

isUnsat :: CNF -> Bool
isUnsat (CNF clauses) = any null clauses

solutionsFromAssignment :: Assignment -> Solutions
solutionsFromAssignment = IntMap.keysSet . IntMap.filter id
{-# INLINEABLE solutionsFromAssignment #-}

simplify :: CNF -> Assignment -> VSIDS -> (CNF, Assignment, VSIDS)
simplify cnf m vsids = (cnf'', m'', vsids) -- TODO, move vsids through
  where
    (cnf', m') = eliminateLiterals cnf m
    (cnf'', m'') = unitPropagate cnf' m'

getSolutions :: CNF -> Maybe Solutions
getSolutions init' = go mempty init' (initWatchedLiterals init') $ initVSIDS init' 
  where
    go :: Assignment -> CNF -> WatchedLiterals -> VSIDS -> Maybe Solutions
    go m1 cnf wl vsids
      | isUnsat cnf' = Nothing
      | isSat cnf' = Just $ solutionsFromAssignment m
      | otherwise = case pickVariable vsids' of
          Nothing -> if isSat cnf' then Just $ solutionsFromAssignment m else Nothing
          Just (c, vsids'')-> tryAssign c True wl vsids'' <|> tryAssign c False wl vsids''
      where
        (cnf', m, vsids') = simplify cnf m1 vsids

        tryAssign :: Int -> Bool -> WatchedLiterals -> VSIDS -> Maybe Solutions
        tryAssign c v wl' vsids'' =
          let newCnf = substitute c v cnf'
              newM = IntMap.insertWith (const id) c v m
           in go newM newCnf wl' (decay vsids'')

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
