{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SAT.Optimisers (unitPropagate, literalElimination, collectLiterals, collectLiteralsToSet, uniqueOnly) where

import Control.Monad (ap)
import Data.Bifunctor (type Bifunctor (first))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (type Set)
import Data.Set qualified as Set
import SAT.Expr (flipPolarity, type Expr (And, Not, Or, Val, Var), type Polarity (Negative, Positive), type SolutionMap)

-- large optimisation would be using int map or int set when expr a a is int. is this possible ?

-- BIG O NOTATION: O(n log n)? (I think)
collectLiterals :: Expr a -> [a]
collectLiterals = foldMap pure
{-# INLINEABLE collectLiterals #-}

-- BIG O NOTATION: O(n log n)
-- this quicker or union sets ?
collectLiteralsToSet :: (Ord a) => Expr a -> Set a
collectLiteralsToSet = Set.fromList . collectLiterals
{-# INLINEABLE collectLiteralsToSet #-}

literalPolarity :: forall a. (Ord a) => a -> Expr a -> (Maybe Polarity, SolutionMap a)
literalPolarity = go mempty
  where
    go :: SolutionMap a -> a -> Expr a -> (Maybe Polarity, SolutionMap a)
    go m c e = case e of
      (Var v) -> handleVar v Positive
      (Not (Var v)) -> handleVar v Negative
      -- pair is monoid by when elems are monoids, Map is monoid under union, Polarity is monoid
      (And e1 e2) -> go m c e1 <> go m c e2
      (Or e1 e2) -> go m c e1 <> go m c e2
      -- is this correct ?
      (Not e') -> first (fmap flipPolarity) $ go m c e'
      (Val _) -> (Nothing, m)
      where
        handleVar :: a -> Polarity -> (Maybe Polarity, SolutionMap a)
        handleVar v polarity
          | c == v = (Just polarity, Map.insert v (polarity == Positive) m)
          | otherwise = (Nothing, m)
{-# INLINEABLE literalPolarity #-}

-- BIG O NOTATION: O(n log n)
eliminateLiteral :: forall a. (Ord a) => a -> Expr a -> Polarity -> (Expr a, SolutionMap a)
eliminateLiteral = go mempty
  where
    go :: SolutionMap a -> a -> Expr a -> Polarity -> (Expr a, SolutionMap a)
    go m c e p = case e of
      (Var v) -> handleVar v p
      (Not (Var v)) -> handleVar v $ flipPolarity p
      (And e1 e2) -> applyToBoth And e1 e2
      (Or e1 e2) -> applyToBoth Or e1 e2
      (Not e') -> first Not $ go m c e' $ flipPolarity p
      (Val _) -> (e, m)
      where
        applyToBoth :: (Expr a -> Expr a -> Expr a) -> Expr a -> Expr a -> (Expr a, SolutionMap a)
        applyToBoth constructor e1 e2 =
          let (e1', s1) = go m c e1 p
              (e2', s2) = go m c e2 p
           in (constructor e1' e2', s1 <> s2)

        handleVar :: a -> Polarity -> (Expr a, SolutionMap a)
        handleVar v polarity
          | c == v && polarity == Positive = (Val True, Map.insert v True m)
          | c == v && polarity == Negative = (Val False, Map.insert v False m)
          | otherwise = (e, m)
{-# INLINEABLE eliminateLiteral #-}

-- eliminateLiteral :: forall a. (Ord a) => a -> Expr a -> Polarity -> (Expr a, Set (a, Bool))
-- eliminateLiteral c e p = case e of
--  (Var v) -> handleVar v p
--  (Not (Var v)) -> handleVar v $ flipPolarity p
--  (And e1 e2) -> applyToBoth And e1 e2
--  (Or e1 e2) -> applyToBoth Or e1 e2
--  (Not e') -> first Not $ eliminateLiteral c e' $ flipPolarity p
--  _ -> (e, mempty)
--  where
--    applyToBoth :: (Expr a -> Expr a -> Expr a) -> Expr a -> Expr a -> (Expr a, Set (a, Bool))
--    applyToBoth constructor e1 e2 =
--      let (e1', s1) = eliminateLiteral c e1 p
--          (e2', s2) = eliminateLiteral c e2 p
--       in (constructor e1' e2', s1 <> s2)
--
--    handleVar :: a -> Polarity -> (Expr a, Set (a, Bool))
--    handleVar v polarity
--      | c == v && polarity == Positive = (Val True, Set.singleton (v, True))
--      | c == v && polarity == Negative = (Val False, Set.singleton (v, False))
--      | otherwise = (e, mempty)

-- BIG O NOTATION: O(n log n)
eliminateLiterals :: forall a. (Ord a) => Expr a -> Set a -> Expr a
eliminateLiterals = foldr go
  where
    go :: a -> Expr a -> Expr a
    go c e' = fst . eliminateLiteral c e' $ fromMaybe (error "Literal not found") (fst $ literalPolarity c e')
{-# INLINEABLE eliminateLiterals #-}

literalElimination :: (Ord a) => Expr a -> Expr a
literalElimination = ap eliminateLiterals collectLiteralsToSet
{-# INLINEABLE literalElimination #-}

-- BIG O NOTATION: O(1)
unitClause :: Expr a -> Maybe (a, Bool)
unitClause (Var c) = Just (c, True)
unitClause (Not (Var c)) = Just (c, False)
unitClause _ = Nothing

-- finds all of the or clauses -> when in cnf form is a list of ORd ANDs, so this collects all the ors
clauses :: Expr a -> [Expr a]
clauses (And e1 e2) = clauses e1 <> clauses e2
clauses e = [e]

-- this finds all the clauses with just one variable, and propagates
allUnitClauses :: Expr a -> [(a, Bool)]
allUnitClauses = mapMaybe unitClause . clauses

unitPropagate :: forall a. (Ord a) => Expr a -> (Expr a, SolutionMap a)
unitPropagate = go mempty
  where
    go :: SolutionMap a -> Expr a -> (Expr a, SolutionMap a)
    -- should i not return the first unit clause found ?
    -- or is it ok due to laziness
    go m e = case allUnitClauses e of
      [] -> (e, m)
      (c', b') : _ ->
        let (newExpr, newMap) = eliminateLiteral c' e $ if b' then Positive else Negative
         in go (Map.union m newMap) newExpr
{-# INLINEABLE unitPropagate #-}

-- https://buffered.io/posts/a-better-nub/
uniqueOnly :: forall a. (Ord a) => [a] -> [a]
uniqueOnly = go mempty
  where
    go :: Set a -> [a] -> [a]
    go _ [] = []
    go s (x : xs)
      | x `Set.member` s = go s xs
      | otherwise = x : go (Set.insert x s) xs
{-# INLINEABLE uniqueOnly #-}
