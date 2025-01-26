{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module SAT.Assignment (type Assignment, type Solutions, checkValue, initAssignment, filterAssignment, solutionsFromAssignment, allAssignments, varValue, assign, literalValue) where

import Control.Parallel.Strategies (type NFData)
import Data.IntMap.Strict (type IntMap, (!?))
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (type IntSet)
import GHC.Generics (type Generic)
import SAT.CNF (Literal, varOfLiteral, isNegative)
import qualified Data.IntSet as IntSet

newtype Assignment = Assignment (IntMap Bool) deriving stock (Eq, Show, Ord, Generic)
-- type Assignment = Vector Bool

deriving anyclass instance NFData Assignment

initAssignment :: IntSet -> Assignment
initAssignment _ = Assignment mempty
{-# INLINEABLE initAssignment #-}

filterAssignment :: (Int -> Bool -> Bool) -> Assignment -> Assignment
filterAssignment f (Assignment m) = Assignment $ IntMap.filterWithKey f m
{-# INLINEABLE filterAssignment #-}

-- | Converts an assignment to a set of solutions.
solutionsFromAssignment :: Assignment -> Solutions
solutionsFromAssignment (Assignment m) = Solutions $ IntMap.keysSet $ IntMap.filter id m
{-# INLINEABLE solutionsFromAssignment #-}

allAssignments :: Assignment -> IntSet
allAssignments (Assignment m) = IntMap.keysSet m
{-# INLINEABLE allAssignments #-}

instance Semigroup Assignment where
  (<>) :: Assignment -> Assignment -> Assignment
  Assignment a <> Assignment b = Assignment $ a <> b
  {-# INLINEABLE (<>) #-}

instance Monoid Assignment where
  mempty :: Assignment
  mempty = Assignment mempty
  {-# INLINEABLE mempty #-}

-- | Returns the value of a variable in an assignment.
varValue :: Assignment -> Int -> Maybe Bool
varValue (Assignment m) = (m !?)
{-# INLINEABLE varValue #-}

-- >>> assign IntMap.empty 1 True 0
-- fromList [(1,True)]
assign :: Assignment -> Literal -> Bool -> Assignment
assign (Assignment m) c v = Assignment $ IntMap.insertWith (const id) (varOfLiteral c) v m
{-# INLINEABLE assign #-}

-- | Returns the value of a literal in an assignment.
literalValue :: Assignment -> Literal -> Maybe Bool
literalValue assignment l = do
  val <- varValue assignment $ varOfLiteral l
  return $ if isNegative l then not val else val
{-# INLINEABLE literalValue #-}

-- | The solutions type.
newtype Solutions = Solutions IntSet deriving stock (Eq, Show, Ord, Generic)

-- | Checks if a value is in the solutions.
--
-- >>> checkValue (Solutions $ IntSet.fromList [1, 2, 3]) 2
-- True
checkValue :: Solutions -> Int -> Bool
checkValue (Solutions s) l = IntSet.member l s
{-# INLINEABLE checkValue #-}