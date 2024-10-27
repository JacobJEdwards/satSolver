{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SAT.Solver
  ( satisfiable,
    getSolutions,
    type Solutions,
    checkValue,
    findFreeVariable,
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe)
import Data.IntSet qualified as IntSet
import SAT.CNF (type Clause, type Literal, type CNF (CNF))
import SAT.Expr (type Solutions)
import SAT.Optimisers (collectLiterals, eliminateLiterals, unitPropagate)
import Debug.Trace (trace)

findFreeVariable ::  CNF -> Maybe Int
findFreeVariable = listToMaybe . collectLiterals
{-# INLINEABLE findFreeVariable #-}

substitute :: Int -> Bool -> CNF -> CNF
substitute var val (CNF clauses) = CNF $ map (eliminateClause var val) $ filter (not . clauseIsTrue var val) clauses
  where
    clauseIsTrue :: Literal -> Bool -> Clause -> Bool
    clauseIsTrue c p = any (literalIsTrue c p)
    
    literalIsTrue :: Literal -> Bool -> Literal -> Bool
    literalIsTrue c p l = case l of
      v | abs c == abs v && v > 0 -> p
      v | abs c == abs v && v < 0 -> not p
      _ -> False
      
    literalIsFalse :: Literal -> Bool -> Literal -> Bool
    literalIsFalse c p l = case l of
      v | abs c == abs v && v > 0 -> not p
      v | abs c == abs v && v < 0 -> p
      _ -> False
    
    eliminateClause :: Literal -> Bool -> Clause -> Clause
    eliminateClause c p = filter (not . literalIsFalse c p)
{-# INLINEABLE substitute #-}

checkValue ::  Solutions -> Int -> Bool
checkValue = flip IntSet.member
{-# INLINEABLE checkValue #-}

getSolutions :: CNF -> Maybe Solutions
getSolutions = go IntSet.empty
  where
    go :: Solutions -> CNF -> Maybe Solutions
    go m1 cnf@(CNF clauses')
      | any null clauses' = Nothing
      | null clauses' = trace ("Found solution: " <> show m) $ Just m
      | otherwise =
          case findFreeVariable cnf' of
            Nothing -> trace ("Found solution: " <> show m) $ Just m
            Just c ->
              tryAssign c True <|> tryAssign c False
      where
        tryAssign c v
          | v = go (IntSet.insert c m) $ substitute c True cnf'
          | otherwise = go m $ substitute c False cnf'

        (cnf', m2) = unitPropagate cnf

        m :: Solutions
        m = IntSet.union m1 m2

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
