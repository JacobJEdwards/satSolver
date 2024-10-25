
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SAT.Old.Optimisers (collectLiterals, unitClause, eliminateLiteral, eliminateLiterals, literalElimination, unitPropagate) 
where

import Control.Monad (ap)
import Data.Bifunctor (type Bifunctor (first))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (type Set)
import Data.Set qualified as Set
import SAT.Expr (type Expr (And, Not, Or, Val, Var), type Solutions)
import SAT.Polarity (flipPolarity, type Polarity (Negative, Positive))

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

literalPolarity :: forall a. (Ord a) => a -> Expr a -> (Maybe Polarity, Solutions a)
literalPolarity = go mempty
  where
    go :: Solutions a -> a -> Expr a -> (Maybe Polarity, Solutions a)
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
        handleVar :: a -> Polarity -> (Maybe Polarity, Solutions a)
        handleVar v polarity
          | c == v = (Just polarity, if polarity == Positive then Set.insert v m else m)
          | otherwise = (Nothing, m)
{-# INLINEABLE literalPolarity #-}

-- BIG O NOTATION: O(n log n)
eliminateLiteral :: forall a. (Ord a) => a -> Expr a -> Polarity -> (Expr a, Solutions a)
eliminateLiteral = go mempty
  where
    go :: Solutions a -> a -> Expr a -> Polarity -> (Expr a, Solutions a)
    go sol c e p = case e of
      (Var v) -> handleVar v p
      (Not (Var v)) -> handleVar v $ flipPolarity p
      (And e1 e2) -> applyToBoth And e1 e2
      (Or e1 e2) -> applyToBoth Or e1 e2
      (Not e') -> first Not $ go sol c e' $ flipPolarity p
      (Val _) -> (e, sol)
      where
        applyToBoth :: (Expr a -> Expr a -> Expr a) -> Expr a -> Expr a -> (Expr a, Solutions a)
        applyToBoth constructor e1 e2 =
          let (e1', s1) = go sol c e1 p
              (e2', s2) = go sol c e2 p
           in (constructor e1' e2', s1 <> s2)

        handleVar :: a -> Polarity -> (Expr a, Solutions a)
        handleVar v polarity
          | c == v && polarity == Positive = (Val True, Set.insert v sol)
          | c == v && polarity == Negative = (Val False, sol)
          | otherwise = (e, sol)
{-# INLINEABLE eliminateLiteral #-}


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

unitPropagate :: forall a. (Ord a) => Expr a -> (Expr a, Solutions a)
unitPropagate = go mempty
  where
    go :: Solutions a -> Expr a -> (Expr a, Solutions a)
    go sol e = case allUnitClauses e of
      [] -> (e, sol)
      (c', b') : _ ->
        let (newExpr, newSol) = eliminateLiteral c' e $ if b' then Positive else Negative
         in go (sol <> newSol) newExpr
{-# INLINEABLE unitPropagate #-}