{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SAT.Optimisers (unitPropagate, literalElimination, substitute, collectLiterals, collectLiteralsToSet, uniqueOnly, eliminateLiterals) where

import Data.Maybe (mapMaybe)
import Data.IntSet (type IntSet)
import Data.IntSet qualified as IntSet
import Data.Set (type Set)
import Data.Set qualified as Set
import SAT.CNF (CNF(CNF), Clause, type Literal)
import SAT.Polarity (type Polarity (Negative, Positive, Mixed))
import Data.IntMap qualified as IntMap
import Data.IntMap(type IntMap)
import Data.List (find)

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


literalPolarities :: CNF -> IntMap Polarity
literalPolarities (CNF clauses') = foldl updatePolarity IntMap.empty (concatMap clausePolarities clauses')
  where
    clausePolarities clause = [(unLit lit, literalPolarity lit) | lit <- clause]
    
    unLit :: Literal -> Int
    unLit = abs
    
    literalPolarity :: Literal -> Polarity
    literalPolarity p
      | p < 0 = Negative 
      | p > 0 = Positive
      | otherwise = Mixed
    
    updatePolarity acc (lit, pol) = IntMap.insertWith (<>) lit pol acc
    
{-# INLINEABLE literalPolarities #-}

eliminateLiterals :: CNF -> IntMap Bool -> (CNF, IntMap Bool)
eliminateLiterals (CNF clauses) solutions = (clauses', solutions')
  where 
    literals :: IntMap Polarity
    literals = IntMap.filter (/= Mixed) $ literalPolarities (CNF clauses)
    
    (clauses', solutions') = getClauses literals (CNF clauses) solutions
    
    getClauses :: IntMap Polarity -> CNF -> IntMap Bool -> (CNF, IntMap Bool)
    getClauses pols expr sols = case IntMap.minViewWithKey pols of
      Nothing -> (expr, sols)
      Just ((c, p), pols') -> getClauses pols' (substitute c value expr) (IntMap.insert c value sols)
        where 
          value :: Bool
          value = p == Positive
{-# INLINEABLE eliminateLiterals #-} -- wrong i think



substitute :: Int -> Bool -> CNF -> CNF
substitute var val (CNF clauses) = CNF $ map (eliminateClause var val) $ filter (not . clauseIsTrue var val) clauses
  where
    clauseIsTrue :: Literal -> Bool -> Clause -> Bool
    clauseIsTrue c p = any (literalIsTrue' c p)
    
    literalIsTrue' :: Literal -> Bool -> Literal -> Bool
    literalIsTrue' c p l = case l of
      v | abs c == abs v && v > 0 -> p
      v | abs c == abs v && v < 0 -> not p
      _ -> False
      
    literalIsFalse' :: Literal -> Bool -> Literal -> Bool
    literalIsFalse' c p l = case l of
      v | abs c == abs v && v > 0 -> not p
      v | abs c == abs v && v < 0 -> p
      _ -> False
    
    eliminateClause :: Literal -> Bool -> Clause -> Clause
    eliminateClause c p = filter (not . literalIsFalse' c p)
{-# INLINEABLE substitute #-}

literalElimination :: CNF -> IntMap Bool -> (CNF, IntMap Bool)
literalElimination = eliminateLiterals
{-# INLINEABLE literalElimination #-}

findUnitClause :: CNF -> Maybe (Int, Bool)
findUnitClause (CNF clauses) = case unitClause of
  Nothing -> Nothing
  Just clause -> let p = head clause in Just (abs p, p > 0)
  where
    unitClause = find isUnitClause clauses
    
    isUnitClause :: Clause -> Bool
    isUnitClause c = length c  == 1 
    
unitPropagate :: CNF -> IntMap Bool -> (CNF, IntMap Bool)
unitPropagate = go
  where
    go :: CNF -> IntMap Bool -> (CNF, IntMap Bool)
    go e m = case findUnitClause e of
      Nothing -> (e, m)
      Just (c, p) ->
        let newExpr = substitute c p e
            newSol = IntMap.insert c p m
         in go newExpr newSol
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
