{-# LANGUAGE ScopedTypeVariables #-}

module SAT.Optimisers (unitPropagate, literalElimination, collectLiterals, collectLiteralsToSet, uniqueOnly) where

import Data.Bifunctor (Bifunctor (first))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import SAT.Expr

collectLiterals :: Expr a -> [a]
collectLiterals = foldMap (: [])

collectLiteralsToSet :: (Ord a) => Expr a -> Set a
collectLiteralsToSet = Set.fromList . collectLiterals

literalPolarity :: forall a. (Ord a) => a -> Expr a -> (Maybe Polarity, Set (a, Bool))
literalPolarity c e = case e of
  (Var v) -> handleVar v Positive
  (Not (Var v)) -> handleVar v Negative
  (And e1 e2) -> literalPolarity c e1 <> literalPolarity c e2
  (Or e1 e2) -> literalPolarity c e1 <> literalPolarity c e2
  (Not e') -> first (fmap flipPolarity) $ literalPolarity c e'
  (Val _) -> mempty
  where
    handleVar :: a -> Polarity -> (Maybe Polarity, Set (a, Bool))
    handleVar v polarity
      | c == v = (Just polarity, Set.singleton (v, polarity == Positive))
      | otherwise = mempty

--  combinePolarities :: (Maybe Polarity, Set (a, Bool)) -> (Maybe Polarity, Set (a, Bool)) -> (Maybe Polarity, Set (a, Bool))
--  combinePolarities = (<>)
-- combinePolarities (p1, s1) (p2, s2) = (p1 <> p2, Set.union s1 s2)

eliminateLiteral :: forall a. (Ord a) => a -> Expr a -> Polarity -> (Expr a, Set (a, Bool))
eliminateLiteral c e p = case e of
  (Var v) -> handleVar v p
  (Not (Var v)) -> handleVar v $ flipPolarity p
  (And e1 e2) -> applyToBoth And e1 e2
  (Or e1 e2) -> applyToBoth Or e1 e2
  (Not e') -> first Not $ eliminateLiteral c e' $ flipPolarity p
  _ -> (e, mempty)
  where
    applyToBoth :: (Expr a -> Expr a -> Expr a) -> Expr a -> Expr a -> (Expr a, Set (a, Bool))
    applyToBoth constructor e1 e2 =
      let (e1', s1) = eliminateLiteral c e1 p
          (e2', s2) = eliminateLiteral c e2 p
       in (constructor e1' e2', s1 <> s2)

    handleVar :: a -> Polarity -> (Expr a, Set (a, Bool))
    handleVar v polarity
      | c == v && polarity == Positive = (Val True, Set.singleton (v, True))
      | c == v && polarity == Negative = (Val False, Set.singleton (v, False))
      | otherwise = (e, mempty)

eliminateLiterals :: forall a. (Ord a) => Expr a -> Set a -> Expr a
eliminateLiterals = foldr eliminate
  where
    eliminate :: a -> Expr a -> Expr a
    eliminate c e' = fst . eliminateLiteral c e' $ fromMaybe (error "Literal not found") (fst $ literalPolarity c e')

literalElimination :: (Ord a) => Expr a -> Expr a
literalElimination e = eliminateLiterals e $ collectLiteralsToSet e

unitClause :: Expr a -> Maybe (a, Bool)
unitClause (Var c) = Just (c, True)
unitClause (Not (Var c)) = Just (c, False)
unitClause _ = Nothing

clauses :: Expr a -> [Expr a]
clauses (And e1 e2) = clauses e1 <> clauses e2
clauses e = [e]

allUnitClauses :: Expr a -> [(a, Bool)]
allUnitClauses = mapMaybe unitClause . clauses

unitPropagate :: forall a. (Ord a) => Expr a -> (Expr a, Set (a, Bool))
unitPropagate e = unitPropagate' e mempty
  where
    unitPropagate' :: Expr a -> Set (a, Bool) -> (Expr a, Set (a, Bool))
    unitPropagate' e' xs = case allUnitClauses e' of
      [] -> (e', xs)
      (c', b') : _ ->
        let newExpr = fst $ eliminateLiteral c' e' $ if b' then Positive else Negative
            newSet = Set.insert (c', b') xs
         in unitPropagate' newExpr newSet

-- https://buffered.io/posts/a-better-nub/
uniqueOnly :: forall a. (Ord a) => [a] -> [a]
uniqueOnly = go mempty
  where
    go :: Set a -> [a] -> [a]
    go _ [] = []
    go s (x : xs)
      | x `Set.member` s = go s xs
      | otherwise = x : go (Set.insert x s) xs
