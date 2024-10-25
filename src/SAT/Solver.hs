{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module SAT.Solver
  ( satisfiable,
    propagateValue,
    toSimple,
    getSolutions,
    type Solutions,
    checkValue,
    findFreeVariable,
  )
where

import Data.Maybe (listToMaybe)
import SAT.CNF (type CNF(CNF), Clause, Literal(Pos, Neg, Const))
import SAT.Expr (type Solutions)
import SAT.Optimisers (literalElimination, unitPropagate, eliminateLiterals, collectLiterals, collectLiteralsToSet)
import Control.Applicative ((<|>))
import Data.Set qualified as Set

findFreeVariable :: forall a. (Ord a, Show a) => CNF a -> Maybe a
findFreeVariable =  listToMaybe . collectLiterals
{-# INLINEABLE findFreeVariable #-}

substitute :: forall a. (Eq a) => a -> Bool -> CNF a -> CNF a
substitute var val (CNF clauses) = CNF $ map substituteClause clauses
  where
    substituteClause :: Clause a -> Clause a
    substituteClause = map substituteLiteral
    
    substituteLiteral :: Literal a -> Literal a
    substituteLiteral (Pos a) 
      | a == var && val = Const True
      | a == var && not val = Const False
      | otherwise = Pos a
    substituteLiteral (Neg a) 
      | a == var && val = Const False
      | a == var && not val = Const True
      | otherwise = Neg a
    substituteLiteral (Const b) = Const b
{-# INLINEABLE substitute #-}

propagateValue :: CNF a -> CNF a
propagateValue (CNF clauses) = CNF $ propagateClauses $ filter (not . isTriviallyTrue) clauses
  where
    propagateClauses :: [Clause a] -> [Clause a]
    propagateClauses clauses'
      | any isTriviallyFalse clauses = [[Const False]]
      | otherwise = map propagateClause clauses'
    
    propagateClause :: Clause a -> Clause a
    propagateClause = filter (not . isFalse)
    
    isTrue :: Literal a -> Bool
    isTrue (Const True) = True
    isTrue _ = False
    
    isFalse :: Literal a -> Bool
    isFalse (Const False) = True
    isFalse _ = False
    
    isTriviallyTrue :: Clause a -> Bool
    isTriviallyTrue = any isTrue 
    
    isTriviallyFalse :: Clause a -> Bool
    isTriviallyFalse = all isFalse
{-# INLINEABLE propagateValue #-}

propagateAndCheck :: CNF a -> Bool
propagateAndCheck = check . propagateValue
  where
    check :: CNF a -> Bool
    check (CNF clauses) = null clauses
    
    isTrue :: Clause a -> Bool
    isTrue = any $ \case
      Const True -> True
      _ -> False
{-# INLINEABLE propagateAndCheck #-}

checkValue :: (Ord a) => Solutions a -> a -> Bool
checkValue = flip Set.member
{-# INLINEABLE checkValue #-}

toSimple :: (Ord a) => CNF a -> CNF a
toSimple = fst . unitPropagate

getSolutions :: forall a. (Ord a, Show a) => CNF a -> Maybe (Solutions a)
getSolutions cnf = let (cnf'', m) = eliminateLiterals cnf in go m $ propagateValue cnf''
  where 
    go :: Solutions a -> CNF a -> Maybe (Solutions a)
    go m cnf' = case findFreeVariable cnf'' of
      Nothing -> if propagateAndCheck cnf'' then Just m'' else Nothing
      Just c ->
        let trueGuess = propagateValue $ substitute c True cnf''
            falseGuess = propagateValue $ substitute c False cnf''
            
            trueSolution = go (Set.insert c m'') trueGuess
            falseSolution = go m'' falseGuess
          in trueSolution <|> falseSolution
      where 
        (cnf'', m') = unitPropagate cnf'
        
        m'' :: Solutions a
        m'' = m <> m'
{-# INLINEABLE getSolutions #-}

satisfiable :: (Ord a, Show a) => CNF a -> Bool
satisfiable cnf = case findFreeVariable cnf' of
  Nothing -> propagateAndCheck cnf'
  Just c ->
    let trueGuess = propagateValue $ substitute c True cnf'
        falseGuess = propagateValue $ substitute c False cnf'
     in satisfiable trueGuess || satisfiable falseGuess
  where
    cnf' = toSimple cnf
{-# INLINEABLE satisfiable #-}