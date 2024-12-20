{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Strict #-}

-- |
-- Module      : SAT.VSIDS
-- Description : Exports the VSIDS module.
module SAT.VSIDS (type VSIDS, initVSIDS, decay, adjustScore, updateScore, decayFactor, adjustScores, pickLiteral) where

import Data.IntMap.Strict (type IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl')
import SAT.CNF (varOfLiteral, type CNF (CNF), type Clause, type Literal)

-- | The VSIDS type.
type VSIDS = IntMap Double

-- | The decay factor.
decayFactor :: Double
decayFactor = 0.96

-- | Decays the scores.
--
-- >>> decay (IntMap.fromList [(1, 1), (2, 2), (3, 3)])
-- WAS fromList [(1,0.99),(2,1.98),(3,2.97)]
-- NOW fromList [(1,0.99),(2,1.98),(3,2.9699999999999998)]
decay :: VSIDS -> VSIDS
decay = IntMap.map (* decayFactor)

-- | Initializes the VSIDS.
--
-- >>> initVSIDS (CNF [[1, 2], [2, 3], [3, 4]])
-- fromList [(1,1.0),(2,2.0),(3,2.0),(4,1.0)]
initVSIDS :: CNF -> VSIDS
initVSIDS (CNF clauses) = foldl' updateVSIDS IntMap.empty clauses
  where
    updateVSIDS :: VSIDS -> Clause -> VSIDS
    updateVSIDS = foldl' \vs l -> IntMap.insertWith (+) (varOfLiteral l) 1 vs

-- | Adjusts the score of a variable.
--
-- >>> adjustScore 1 (IntMap.fromList [(1, 1), (2, 2), (3, 3)])
-- fromList [(1,2.0),(2,2.0),(3,3.0)]
adjustScore :: Literal -> VSIDS -> VSIDS
adjustScore l = IntMap.insertWith (+) (varOfLiteral l) 1
{-# INLINEABLE adjustScore #-}

-- | Updates the score of a variable.
--
-- >>> updateScore 1 2 (IntMap.fromList [(1, 1), (2, 2), (3, 3)])
-- fromList [(1,2.0),(2,2.0),(3,3.0)]
updateScore :: Literal -> Double -> VSIDS -> VSIDS
updateScore l = IntMap.insert $ varOfLiteral l
{-# INLINEABLE updateScore #-}

adjustScores :: VSIDS -> Clause -> VSIDS
adjustScores = foldl' $ flip adjustScore
{-# INLINEABLE adjustScores #-}

pickLiteral :: VSIDS -> Literal
pickLiteral = fst . IntMap.findMax
{-# INLINEABLE pickLiteral #-}
