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
import Data.IntMap.Strict (type IntMap)
import SAT.CNF (varOfLiteral, type Clause (Clause, literals, watched), type Literal)
import SAT.Monad (type WatchedLiterals (WatchedLiterals), type ClauseDB)
import Data.IntSet (type IntSet)
import Data.IntSet qualified as IntSet
import Data.Sequence qualified as Seq

initWatchedLiterals :: ClauseDB -> WatchedLiterals
initWatchedLiterals clauseDB = WatchedLiterals litMap
  where
    accumulate :: IntMap IntSet -> (Int, Clause) -> IntMap IntSet
    accumulate acc (idx, Clause {watched = (a, b), literals}) =
      if a == b
        then 
          IntMap.insertWith (<>) (varOfLiteral $ literals !! a) (IntSet.singleton idx) acc
        else
          IntMap.insertWith (<>) (varOfLiteral $ literals !! a) (IntSet.singleton idx) $
            IntMap.insertWith (<>) (varOfLiteral $ literals !! b) (IntSet.singleton idx) acc

    litMap :: IntMap.IntMap IntSet
    litMap = foldl' accumulate mempty (Seq.zip (Seq.fromList [0..length clauseDB]) clauseDB)

initClauseWatched :: Clause -> Clause
initClauseWatched (Clause {literals}) = Clause {literals = literals, watched = (a, b)}
  where

    a :: Int
    b :: Int
    (a, b) = findInitialWatched literals

    findInitialWatched :: [Literal] -> (Int, Int)
    findInitialWatched lits = do 
      let len = length lits
      if len == 1
        then (0, 0)
        else (0, 1)
