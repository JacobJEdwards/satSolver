{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}

module SAT.VSIDS (type VSIDS, initVSIDS, decay, pickVariable, adjustScore, updateScore, decayFactor) where
  
import Data.IntMap (type IntMap)
import Data.IntMap qualified as IntMap
import SAT.CNF (type CNF(CNF), type Clause, type Literal)
import Data.List (foldl')

type VSIDS = IntMap Double

decayFactor :: Double
decayFactor = 0.99

decay :: VSIDS -> VSIDS
decay = IntMap.map (* decayFactor)

initVSIDS :: CNF -> VSIDS
initVSIDS (CNF clauses) = foldl' updateVSIDS IntMap.empty clauses
  where
    updateVSIDS :: VSIDS -> Clause -> VSIDS
    updateVSIDS = foldl' (\vsids' l -> IntMap.insertWith (+) (abs l) 1 vsids')
    
    
pickVariable :: VSIDS -> Maybe (Literal, VSIDS)
pickVariable vs = case IntMap.maxViewWithKey vs of
  Just ((k, _), vsids') -> Just (k, vsids')
  Nothing -> Nothing
{-# INLINEABLE pickVariable #-}

adjustScore :: Literal -> VSIDS -> VSIDS
adjustScore l = IntMap.insertWith (+) (abs l) 1
{-# INLINEABLE adjustScore #-}

updateScore :: Literal -> Double -> VSIDS -> VSIDS
updateScore l = IntMap.insert (abs l)
{-# INLINEABLE updateScore #-}
