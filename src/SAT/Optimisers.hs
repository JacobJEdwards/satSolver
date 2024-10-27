{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SAT.Optimisers (unitPropagate, literalElimination, substitute, collectLiterals, collectLiteralsToSet, uniqueOnly, eliminateLiterals) where

import Data.Maybe (mapMaybe, catMaybes)
import Data.IntSet (type IntSet)
import Data.IntSet qualified as IntSet
import Data.Set (type Set)
import Data.Set qualified as Set
import SAT.Expr (type Solutions)
import SAT.CNF (CNF(CNF), Clause, type Literal)
import SAT.Polarity (type Polarity (Negative, Positive, Mixed))
import qualified Data.Map as Map
import Debug.Trace (trace)

-- large optimisation would be using int map or int set when expr a a is int. is this possible ?

collectLiterals :: CNF -> [Int]
collectLiterals (CNF clauses') = concatMap getVars clauses'
  where
    getVars :: Clause -> [Int]
    getVars = map abs
{-# INLINEABLE collectLiterals #-}

collectLiteralsToSet :: CNF -> IntSet
collectLiteralsToSet = IntSet.fromList . collectLiterals
{-# INLINEABLE collectLiteralsToSet #-}


literalPolarities :: CNF -> [(Int, Polarity)]
literalPolarities (CNF clauses') = Map.toList $ foldl updatePolarity Map.empty (concatMap clausePolarities clauses')
  where
    clausePolarities clause = [(unLit lit, literalPolarity lit) | lit <- clause]
    
    unLit :: Literal -> Int
    unLit = abs
    
    literalPolarity :: Literal -> Polarity
    literalPolarity p
      | p < 0 = Negative 
      | p > 0 = Positive
      | otherwise = Mixed
    
    updatePolarity acc (lit, pol) = Map.insertWith (<>) lit pol acc
    
{-# INLINEABLE literalPolarities #-}

eliminateLiterals :: CNF -> (CNF, Solutions)
eliminateLiterals (CNF clauses) = (clauses', mconcat solutions)
  where 
    literals :: [(Int, Polarity)]
    literals = filter (\(_, p) -> p /= Mixed) $ literalPolarities (CNF clauses)
    
    
    solutions = mapMaybe (\(v, p) -> if p == Positive then Just (IntSet.singleton v) else Nothing) literals
    
    clauses' = getClauses literals (CNF clauses)
    
    getClauses :: [(Int, Polarity)] -> CNF -> CNF
    getClauses [] acc = acc
    getClauses ((c, p) : xs) acc = getClauses xs (substitute c (p == Positive) acc)
{-# INLINEABLE eliminateLiterals #-} -- wrong i think



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

literalElimination :: CNF -> (CNF, Solutions)
literalElimination = eliminateLiterals
{-# INLINEABLE literalElimination #-}

unitClause :: Clause -> Maybe (Int, Bool)
unitClause clause = case clause of
  [literal] -> case literal of
    c | c > 0 -> Just (abs c, True)
    c | c < 0 -> Just (abs c, False)
    _ -> Nothing
  _ -> Nothing
{-# INLINEABLE unitClause #-}

allUnitClauses :: CNF -> [(Int, Bool)]
allUnitClauses (CNF clauses') = mapMaybe unitClause clauses'
{-# INLINEABLE allUnitClauses #-}

unitPropagate :: CNF -> (CNF, Solutions)
unitPropagate = go mempty
  where
    go :: Solutions -> CNF -> (CNF, Solutions)
    go m e = case allUnitClauses e of
      [] -> (e, m)
      (c, p) : _ ->
        let newExpr = substitute c p e
            newSol = if p then IntSet.insert c m else m
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
