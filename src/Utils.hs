{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Utils (hashNub, intNub, unstableHashNub, unstableIntNub) where

import Data.Hashable (type Hashable)
import Data.HashSet qualified as HashSet
import Data.IntSet qualified as IntSet

-- | Like 'Prelude.nub' but runs in @O(n * log_16(n))@ time and requires 'Hashable'.
--
-- >>> hashNub [3, 3, 3, 2, 2, -1, 1]
-- [3, 2, -1, 1]
hashNub :: (Hashable a) => [a] -> [a]
hashNub = go HashSet.empty
  where
    go _ []     = []
    go s (x:xs) =
      if x `HashSet.member` s
      then go s xs
      else x : go (HashSet.insert x s) xs

intNub :: [Int] -> [Int]
intNub = go IntSet.empty
  where
    go _ []     = []
    go s (x:xs) =
      if x `IntSet.member` s
      then go s xs
      else x : go (IntSet.insert x s) xs

-- | Like 'hashNub' but has better performance and also doesn't save the order.
--
-- >>> unstableHashNub [3, 3, 3, 2, 2, -1, 1]
-- [1, 2, 3, -1]
unstableHashNub :: (Hashable a) => [a] -> [a]
unstableHashNub = HashSet.toList . HashSet.fromList

unstableIntNub :: [Int] -> [Int]
unstableIntNub = IntSet.toList . IntSet.fromList
