{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module SAT.Optimisers (unitPropagate, literalElimination, collectLiterals, collectLiteralsToSet, uniqueOnly, eliminateLiterals) where

import Data.Set (type Set)
import Data.Set qualified as Set
import SAT.Expr (type Solutions)
import SAT.CNF (CNF(CNF), Clause, Literal (Const, Neg, Pos))
import SAT.Polarity (type Polarity (Negative, Positive, Mixed))
import Data.Map qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V

-- large optimisation would be using int map or int set when expr a a is int. is this possible ?

collectLiterals :: CNF a -> [a]
collectLiterals (CNF clauses') = concatMap getVars clauses'
  where
    getVars :: Clause a -> [a]
    getVars = concatMap getLit

    getLit :: Literal a -> [a]
    getLit (Pos a) = [a]
    getLit (Neg a) = [a]
    getLit (Const _) = []
{-# INLINEABLE collectLiterals #-}

collectLiteralsToSet :: (Ord a) => CNF a -> Set a
collectLiteralsToSet = Set.fromList . collectLiterals
{-# INLINEABLE collectLiteralsToSet #-}


literalPolarities :: forall a. (Ord a) => CNF a -> Vector (a, Polarity)
literalPolarities (CNF clauses') = V.fromList $ Map.toList $ V.foldl' updatePolarity Map.empty clausePolarities
  where
    clausePolarities = V.concatMap getClausePolarities clauses'
    
    getClausePolarities :: Clause a -> Vector (a, Polarity)
    getClausePolarities = V.mapMaybe getLitPolarity
    
    getLitPolarity :: Literal a -> Maybe (a, Polarity)
    getLitPolarity (Pos a) = Just (a, Positive)
    getLitPolarity (Neg a) = Just (a, Negative)
    getLitPolarity (Const _) = Nothing
    
    updatePolarity acc (lit, pol) = Map.insertWith (<>) lit pol acc
    
{-# INLINEABLE literalPolarities #-}

eliminateLiterals :: forall a. (Ord a) => CNF a -> (CNF a, Solutions a)
eliminateLiterals (CNF clauses) = (clauses', Set.unions solutions)
  where 
    literals :: Vector (a, Polarity)
    literals = literalPolarities (CNF clauses)
    
    solutions :: Vector (Solutions a)
    solutions = V.mapMaybe (\(v, p) -> if p == Positive then Just (Set.singleton v) else Nothing) literals
    
    clauses' = getClauses literals (CNF clauses)
    
    getClauses :: Vector (a, Polarity) -> CNF a -> CNF a
    getClauses pols acc = V.foldl' (\acc' (c, p) -> eliminateLiteral c acc' p) acc pols
{-# INLINEABLE eliminateLiterals #-}

eliminateLiteral :: forall a. (Ord a) => a -> CNF a -> Polarity -> CNF a
eliminateLiteral = go
  where 
    go ::  a -> CNF a -> Polarity -> CNF a
    go c (CNF clauses') p = if p == Mixed then CNF clauses' else CNF $ V.map (eliminateClause c p) clauses'
    
    eliminateClause :: a -> Polarity -> Clause a -> Clause a
    eliminateClause c p = V.map (eliminateLiteral' c p) 
    
    eliminateLiteral' :: a -> Polarity -> Literal a -> Literal a
    eliminateLiteral' c p l = case l of
      Pos v | c == v -> Const (p == Positive)
      Neg v | c == v -> Const (p == Negative)
      _ -> l
{-# INLINEABLE eliminateLiteral #-}

literalElimination :: (Ord a) => CNF a -> (CNF a, Solutions a)
literalElimination = eliminateLiterals
{-# INLINEABLE literalElimination #-}

unitClause :: Clause a -> Maybe (a, Polarity)
unitClause clause 
  | V.length clause == 1 = case V.head clause of
    Pos c -> Just (c, Positive)
    Neg c -> Just (c, Negative)
    _ -> Nothing 
  | otherwise = Nothing
{-# INLINEABLE unitClause #-}

allUnitClauses :: CNF a -> Vector (a, Polarity)
allUnitClauses (CNF clauses') = V.mapMaybe unitClause clauses'
{-# INLINEABLE allUnitClauses #-}

unitPropagate :: forall a. (Ord a) => CNF a -> (CNF a, Solutions a)
unitPropagate = go mempty
  where
    go :: Solutions a -> CNF a -> (CNF a, Solutions a)
    go m e 
      | V.null clauses = (e, m)
      | otherwise = let (c, p) = V.head clauses 
                        newExpr = eliminateLiteral c e p
                        newSol = if p == Positive then Set.insert c m else m
                     in go newSol newExpr
      where 
        clauses = allUnitClauses e
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
