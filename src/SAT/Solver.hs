{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SAT.Solver
  ( satisfiable,
    propagateValue,
    toSimple,
    getSolutions,
    type SolutionMap,
    checkValue,
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import SAT.CNF (toCNF)
import SAT.Expr (unVal, type Expr (And, Not, Or, Val, Var), type SolutionMap)
import SAT.Optimisers (collectLiterals, literalElimination, unitPropagate)

-- findFreeVariable :: Expr a -> Maybe a
-- findFreeVariable (Var c) = Just c
-- findFreeVariable (Not e) = findFreeVariable e
-- findFreeVariable (And e1 e2) = findFreeVariable e1 <|> findFreeVariable e2
-- findFreeVariable (Or e1 e2) = findFreeVariable e1 <|> findFreeVariable e2
-- findFreeVariable (Val _) = Nothing

-- due to haskell's laziness, this function will return the first free variable it finds (but check this is the case)
-- big o notation: O(n)
findFreeVariable :: Expr a -> Maybe a
findFreeVariable = listToMaybe . collectLiterals

-- most expensive operation of course (90 somethings % during profiling)
substitute :: forall a. (Eq a) => a -> Bool -> Expr a -> Expr a
substitute var val expr = expr >>= replace
  where
    replace :: a -> Expr a
    replace c
      | c == var = Val val
      | otherwise = Var c
{-# INLINEABLE substitute #-}

-- propagate all of the values upwards (is there a type class for this?)
propagateValue :: Expr a -> Expr a
propagateValue (Var c) = Var c
propagateValue (Not e) = case propagateValue e of
  Val b -> Val $ not b
  e' -> Not e'
propagateValue (Or e1 e2) = case (propagateValue e1, propagateValue e2) of
  (Val False, e') -> e'
  (e', Val False) -> e'
  (_, Val True) -> Val True
  (Val True, _) -> Val True
  (e1', e2') -> Or e1' e2'
propagateValue (And e1 e2) = case (propagateValue e1, propagateValue e2) of
  (Val True, e') -> e'
  (e', Val True) -> e'
  (Val False, _) -> Val False
  (_, Val False) -> Val False
  (e1', e2') -> And e1' e2'
propagateValue (Val b) = Val b

propagateAndCheck :: Expr a -> Bool
propagateAndCheck = unVal . propagateValue

toSimple :: (Ord a) => Expr a -> Expr a
toSimple = literalElimination . toCNF . fst . unitPropagate
{-# INLINEABLE toSimple #-}

getSolutions :: forall a. (Ord a) => Expr a -> Maybe (SolutionMap a)
getSolutions = uncurry go . unitPropagate
  where
    go :: Expr a -> SolutionMap a -> Maybe (SolutionMap a)
    go expr'' m' = case findFreeVariable expr'' of
      Nothing -> if propagateAndCheck expr'' then Just m' else Nothing
      Just c ->
        let trueGuess = propagateValue $ substitute c True expr''
            falseGuess = propagateValue $ substitute c False expr''
            trueSolution = go trueGuess $ Map.insert c True m'
            falseSolution = go falseGuess $ Map.insert c False m'
         in case (trueSolution, falseSolution) of
              (Just sol, _) -> Just sol
              (_, Just sol) -> Just sol
              _ -> Nothing
{-# INLINEABLE getSolutions #-}

checkValue :: (Ord a) => SolutionMap a -> a -> Bool
checkValue = flip $ Map.findWithDefault False

-- test effect of strictness at some point
satisfiable :: (Ord a, Show a) => Expr a -> Bool
satisfiable !expr = case findFreeVariable expr' of
  Nothing -> propagateAndCheck expr'
  Just c ->
    let trueGuess = propagateValue $ substitute c True expr'
        falseGuess = propagateValue $ substitute c False expr'
     in satisfiable trueGuess || satisfiable falseGuess
  where
    !expr' = toSimple expr
{-# INLINEABLE satisfiable #-}
