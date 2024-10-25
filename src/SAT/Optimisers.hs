{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SAT.Optimisers (unitPropagate, literalElimination, collectLiterals, collectLiteralsToSet, uniqueOnly, eliminateLiterals) where

import Data.Maybe (mapMaybe)
import Data.Set (type Set)
import Data.Set qualified as Set
import SAT.Expr (type Solutions)
import SAT.CNF (CNF(CNF), Clause, Literal (Const, Neg, Pos))
import SAT.Polarity (type Polarity (Negative, Positive, Mixed))
import qualified Data.Map as Map

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


literalPolarities :: forall a. (Ord a) => CNF a -> [(a, Polarity)]
literalPolarities (CNF clauses') = Map.toList $ foldl updatePolarity Map.empty (concatMap clausePolarities clauses')
  where
    clausePolarities clause = [(unLit lit, literalPolarity lit) | lit <- clause, notConst lit]
    
    notConst (Const _) = False
    notConst _ = True
    
    unLit :: Literal a -> a
    unLit (Pos a) = a
    unLit (Neg a) = a
    unLit (Const _) = error "Const literal in clause"

    literalPolarity (Pos _) = Positive
    literalPolarity (Neg _) = Negative
    literalPolarity (Const _) = Positive
    
    updatePolarity acc (lit, pol) = Map.insertWith (<>) lit pol acc
    
{-# INLINEABLE literalPolarities #-}

eliminateLiterals :: forall a. (Ord a) => CNF a -> (CNF a, Solutions a)
eliminateLiterals (CNF clauses) = (clauses', mconcat solutions)
  where 
    literals :: [(a, Polarity)]
    literals = literalPolarities (CNF clauses)
    
    solutions = mapMaybe (\(v, p) -> if p == Positive then Just (Set.singleton v) else Nothing) literals
    
    clauses' = getClauses literals (CNF clauses)
    
    getClauses :: [(a, Polarity)] -> CNF a -> CNF a
    getClauses [] acc = acc
    getClauses ((c, p) : xs) acc = getClauses xs (eliminateLiteral c acc p)
{-# INLINEABLE eliminateLiterals #-}

eliminateLiteral :: forall a. (Ord a) => a -> CNF a -> Polarity -> CNF a
eliminateLiteral = go
  where 
    go ::  a -> CNF a -> Polarity -> CNF a
    go c (CNF clauses') p = if p == Mixed then CNF clauses' else CNF $ map (eliminateClause c p) clauses'
    
    eliminateClause :: a -> Polarity -> Clause a -> Clause a
    eliminateClause c p = map (eliminateLiteral' c p) 
    
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
unitClause clause = case clause of
  [] -> Nothing
  [literal] -> case literal of
    Pos c -> Just (c, Positive)
    Neg c -> Just (c, Negative)
    _ -> Nothing
  _ -> Nothing
{-# INLINEABLE unitClause #-}

allUnitClauses :: CNF a -> [(a, Polarity)]
allUnitClauses (CNF clauses') = mapMaybe unitClause clauses'
{-# INLINEABLE allUnitClauses #-}

unitPropagate :: forall a. (Ord a) => CNF a -> (CNF a, Solutions a)
unitPropagate = go mempty
  where
    go :: Solutions a -> CNF a -> (CNF a, Solutions a)
    go m e = case allUnitClauses e of
      [] -> (e, m)
      (c, p) : _ ->
        let newExpr = eliminateLiteral c e p
            newSol = if p == Positive then Set.insert c m else m
         in go newSol newExpr
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
