{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : SAT.VSIDS
-- Description : Exports the VSIDS module.
module SAT.VSIDS (type VSIDS(VSIDS), initVSIDS, decay, adjustScore, updateScore, decayFactor, adjustScores, pickLiteral, pickVariable) where

import Data.IntMap.Strict (type IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl')
import SAT.CNF (varOfLiteral, type CNF (CNF), type Clause (Clause, literals), type Literal)
import Control.Parallel.Strategies (NFData)

-- | The VSIDS type.
newtype (Num a) => VSIDS a = VSIDS (IntMap a) deriving newtype (Show, Eq, Ord, Semigroup, Monoid, NFData, Functor, Foldable)

-- | The decay factor.
decayFactor :: (Fractional a) => a
decayFactor = 0.99
{-# INLINEABLE decayFactor #-}

-- | Decays the scores.
--
-- >>> decay (IntMap.fromList [(1, 1), (2, 2), (3, 3)])
-- WAS fromList [(1,0.99),(2,1.98),(3,2.97)]
-- NOW fromList [(1,0.99),(2,1.98),(3,2.9699999999999998)]
decay :: (Fractional a) => VSIDS a -> VSIDS a
decay = fmap (* decayFactor)
{-# INLINEABLE decay #-}

-- | Initializes the VSIDS.
--
-- >>> initVSIDS (CNF [[1, 2], [2, 3], [3, 4]])
-- fromList [(1,1.0),(2,2.0),(3,2.0),(4,1.0)]
initVSIDS :: forall a.(Num a) => CNF -> VSIDS a
initVSIDS (CNF !clauses) = foldl' updateVSIDS mempty clauses
  where
    updateVSIDS :: (Num a) => VSIDS a -> Clause -> VSIDS a
    updateVSIDS (VSIDS m) (Clause {literals}) = VSIDS $ foldl' (\vs l -> IntMap.insertWith (+) l 1 vs) m literals
{-# INLINEABLE initVSIDS #-}

-- >>> pickVariable (IntMap.fromList [(1, 1), (2, 2), (3, 3)])
-- Just (3,fromList [(1,1.0),(2,2.0)])
pickVariable :: (Num a) => VSIDS a -> Maybe (Literal, VSIDS a)
pickVariable (VSIDS !vs) = do
  ((!k, _), !vsids') <- IntMap.maxViewWithKey vs
  return (k, VSIDS vsids')
{-# INLINEABLE pickVariable #-}

-- | Adjusts the score of a variable.
--
-- >>> adjustScore 1 (IntMap.fromList [(1, 1), (2, 2), (3, 3)])
-- fromList [(1,2.0),(2,2.0),(3,3.0)]
adjustScore :: (Num a) => Literal -> VSIDS a -> VSIDS a
adjustScore !l (VSIDS !v)= VSIDS $ IntMap.insertWith (+) (varOfLiteral l) 1 v
{-# INLINEABLE adjustScore #-}

-- | Updates the score of a variable.
--
-- >>> updateScore 1 2 (IntMap.fromList [(1, 1), (2, 2), (3, 3)])
-- fromList [(1,2.0),(2,2.0),(3,3.0)]
updateScore :: (Num a) => Literal -> a -> VSIDS a -> VSIDS a
updateScore !l !d (VSIDS !v)= VSIDS $ IntMap.insert (varOfLiteral l) d v
{-# INLINEABLE updateScore #-}

adjustScores :: (Num a) => VSIDS a -> [Literal] -> VSIDS a
adjustScores = foldl' (flip adjustScore)
{-# INLINEABLE adjustScores #-}

pickLiteral :: (Num a) => VSIDS a -> Literal
pickLiteral (VSIDS !v)= fst . IntMap.findMax $ v
{-# INLINEABLE pickLiteral #-}
