{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module SAT.WL
  ( initWatchedLiterals,
    initClauseWatched,
  )
where

import Data.Foldable (foldl')
import Data.IntMap.Strict qualified as IntMap
import Data.List (nub)
import Data.Sequence (type Seq)
import Data.Sequence qualified as Seq
import SAT.CNF (varOfLiteral, type Clause (Clause, literals, watched), type Literal)
import SAT.Monad (type WatchedLiterals (WatchedLiterals), type ClauseDB)
import Data.IntSet (type IntSet)
import Data.IntSet qualified as IntSet

initWatchedLiterals :: ClauseDB -> WatchedLiterals
initWatchedLiterals clauseDB = WatchedLiterals litMap
  where
    accumulate :: (Int, IntMap.IntMap IntSet) -> Clause -> (Int, IntMap.IntMap IntSet)
    accumulate (cnt, !acc) (Clause {watched = (a, b), literals}) =
      if a == b
        then 
          (cnt + 1, IntMap.insertWith (<>) (varOfLiteral $ literals !! a) (IntSet.singleton cnt) acc)
        else
          (cnt + 1, IntMap.insertWith (<>) (varOfLiteral $ literals !! a) (IntSet.singleton cnt) $
            IntMap.insertWith (<>) (varOfLiteral $ literals !! b) (IntSet.singleton cnt) acc)

    litMap :: IntMap.IntMap IntSet
    litMap = snd $ foldl' accumulate (0, mempty) clauseDB

initClauseWatched :: Clause -> Clause
initClauseWatched (Clause {literals}) = Clause {literals = literals', watched = (a, b)}
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
