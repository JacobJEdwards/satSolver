{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module SAT.Assignment (type Assignment, type Solutions, checkValue, initAssignment, solutionsFromAssignment, allAssignments, varValue, assign, literalValue) where

import Control.Parallel.Strategies (type NFData)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (type IntSet)
import GHC.Generics (type Generic)
import SAT.CNF (Literal, varOfLiteral, isNegative)
import qualified Data.IntSet as IntSet
import Data.Vector.Unboxed qualified as Vector

data VarValue = Unassigned | Assigned Bool
  deriving stock (Eq, Show, Ord, Generic)

deriving anyclass instance NFData VarValue

instance Enum VarValue where
  fromEnum :: VarValue -> Int
  fromEnum Unassigned = 0
  fromEnum (Assigned False) = 1
  fromEnum (Assigned True) = 2

  toEnum :: Int -> VarValue
  toEnum 0 = Unassigned
  toEnum 1 = Assigned False
  toEnum 2 = Assigned True
  toEnum _ = error "toEnum: out of bounds"


newtype Assignment = Assignment (Vector.Vector Int) 
  deriving stock (Eq, Show, Ord, Generic)
-- type Assignment = Vector Bool

deriving anyclass instance NFData Assignment

initAssignment :: IntSet -> Assignment
initAssignment vars = Assignment $ Vector.replicate (IntSet.size vars + 1) (fromEnum Unassigned)
{-# INLINEABLE initAssignment #-}

-- | Converts an assignment to a set of solutions.
solutionsFromAssignment :: Assignment -> Solutions
solutionsFromAssignment (Assignment m) = Solutions $ IntMap.keysSet $ Vector.ifoldl' (\acc i v -> if v == fromEnum (Assigned True) then IntMap.insert i () acc else acc) IntMap.empty m
{-# INLINEABLE solutionsFromAssignment #-}

allAssignments :: Assignment -> IntSet
allAssignments (Assignment m) = Vector.ifoldl' (\acc i v -> if v /= fromEnum Unassigned then IntSet.insert i acc else acc) IntSet.empty m
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
varValue (Assignment m) var = do 
  let val = (toEnum $ m Vector.! var) :: VarValue

  case val of
    Unassigned -> Nothing
    Assigned False -> Just False
    Assigned True -> Just True

{-# INLINEABLE varValue #-}

-- >>> assign IntMap.empty 1 True 0
-- fromList [(1,True)]
assign :: Assignment -> Literal -> Bool -> Assignment
assign (Assignment m) c v = Assignment $ m Vector.// [(varOfLiteral c, fromEnum $ Assigned v)]
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