{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Strict #-}

module SAT.Restarts (computeNextLubyThreshold, luby, increaseLubyCount) where

import Control.Monad.RWS.Strict (modify)
import SAT.Monad (SolverM, lubyCount)

scale :: Int
scale = 10

computeNextLubyThreshold :: Int -> Int
computeNextLubyThreshold = (scale *) . luby

luby :: Int -> Int
luby i = go i 1
  where
    go :: Int -> Int -> Int
    go n size
      | n == size = 1
      | n >= 2 * size = go (n - size) (2 * size)
      | otherwise = 2 * go (n - (size `div` 2)) (size `div` 2)

increaseLubyCount :: SolverM ()
increaseLubyCount = modify \s -> s {lubyCount = lubyCount s + 1}
