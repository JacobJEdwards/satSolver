{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module SAT.WL
  ( initWatchedLiterals,
    initClauseWatched,
  )
where

import SAT.CNF (varOfLiteral, type Clause (Clause, literals, watched), type Literal)
import SAT.Monad (type WatchedLiterals (WatchedLiterals), type ClauseDB)
import Data.IntSet qualified as IntSet
import Data.Sequence qualified as Seq
import Data.Vector.Unboxed qualified as VU
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Control.Monad.ST(runST)
import Control.Monad(forM_, when)
import Data.IntSet(type IntSet)

initWatchedLiterals :: ClauseDB -> IntSet.IntSet -> WatchedLiterals
initWatchedLiterals clauseDB vars = WatchedLiterals litMap
  where
    litMap :: V.Vector IntSet
    litMap = runST $ do
      let numVars = IntSet.size vars + 1
      mVec <- MV.replicate numVars IntSet.empty

      let len = Seq.length clauseDB - 1
      let indices = Seq.fromList [0 .. len]

      forM_ (Seq.zip indices clauseDB) $ \(idx, Clause {watched = (a, b), literals}) -> 
        when (length literals > 1) $ do

          let aLit = varOfLiteral (literals !! a)
              bLit = varOfLiteral (literals !! b)

          curAs <- MV.read mVec aLit
          curBs <- MV.read mVec bLit

          MV.write mVec aLit (IntSet.insert idx curAs)
          MV.write mVec bLit (IntSet.insert idx curBs)


      V.freeze mVec


initClauseWatched :: Clause -> Clause
initClauseWatched (Clause {literals}) = Clause {literals = literals, watched = (a, b)}
  where

    a :: Int
    b :: Int
    (a, b) = findInitialWatched literals

    findInitialWatched :: [Literal] -> (Int, Int)
    findInitialWatched lits = do 
      if length lits == 1
        then (0, 0)
        else (0, 1)
