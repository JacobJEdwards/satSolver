{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module SAT.WL
  ( initWatchedLiterals,
    initClauseWatched,
  )
where

import Data.Foldable (foldl')
import Data.IntMap.Strict qualified as IntMap
import SAT.CNF (type CNF (CNF), type Clause(Clause, watched, literals), type Literal, varOfLiteral)
import SAT.Monad (type WatchedLiterals (WatchedLiterals))
import Data.List (nub)
import Data.Sequence (type Seq)
import Data.Sequence qualified as Seq

initWatchedLiterals :: CNF -> WatchedLiterals
initWatchedLiterals (CNF !cs) = WatchedLiterals litMap
  where
    accumulate :: IntMap.IntMap (Seq Clause) -> Clause -> IntMap.IntMap (Seq Clause)
    accumulate acc clause@(Clause {watched = (a, b), literals}) =
      if a == b
        then IntMap.insertWith (<>) (varOfLiteral $ literals !! a) (Seq.singleton clause) acc
        else
          IntMap.insertWith (<>) (varOfLiteral $ literals !! a) (Seq.singleton clause) $
            IntMap.insertWith (<>) (varOfLiteral $ literals !! b) (Seq.singleton clause) acc

    litMap :: IntMap.IntMap (Seq Clause)
    litMap = foldl' accumulate IntMap.empty cs

initClauseWatched :: Clause -> Clause
initClauseWatched (Clause {literals}) = Clause {literals=literals', watched = (a, b)}
  where
    literals' = nub literals

    a :: Int
    b :: Int
    (a, b) = findInitialWatched literals'

    findInitialWatched :: [Literal] -> (Int, Int)
    findInitialWatched lits 
                        | length lits == 1 = (0, 0)
                        | length lits > 1 = (0, 1)
                        | otherwise = error "Empty clause"
