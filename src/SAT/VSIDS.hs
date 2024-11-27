{-|
Module      : SAT.VSIDS
Description : Exports the VSIDS module.
-}

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}

module SAT.VSIDS (type VSIDS, initVSIDS, decay, adjustScore, updateScore, decayFactor, adjustScores, pickLiteral) where

import Data.IntMap (type IntMap)
import Data.IntMap qualified as IntMap
import SAT.CNF (type CNF(CNF), type Clause, type Literal)
import Data.List (foldl')

-- | The VSIDS type.
type VSIDS = IntMap Double

-- | The decay factor.
decayFactor :: Double
decayFactor = 0.95

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
    updateVSIDS = foldl' (\vsids' l -> IntMap.insertWith (+) (abs l) 1 vsids')
    -- updateVSIDS = foldl' (\vsids' l -> IntMap.insert (abs l) 1 vsids')


-- | Adjusts the score of a variable.
-- 
-- >>> adjustScore 1 (IntMap.fromList [(1, 1), (2, 2), (3, 3)])
-- fromList [(1,2.0),(2,2.0),(3,3.0)]
adjustScore :: Literal -> VSIDS -> VSIDS
adjustScore l = IntMap.insertWith (+) (abs l) 1
{-# INLINEABLE adjustScore #-}

-- | Updates the score of a variable.
-- 
-- >>> updateScore 1 2 (IntMap.fromList [(1, 1), (2, 2), (3, 3)])
-- fromList [(1,2.0),(2,2.0),(3,3.0)]
updateScore :: Literal -> Double -> VSIDS -> VSIDS
updateScore l = IntMap.insert (abs l)
{-# INLINEABLE updateScore #-}

adjustScores :: Clause -> VSIDS -> VSIDS
adjustScores clause vsids = foldl' (flip adjustScore) vsids clause
{-# INLINEABLE adjustScores #-}

pickLiteral :: VSIDS -> Literal
pickLiteral = fst . IntMap.findMax
{-# INLINEABLE pickLiteral #-}