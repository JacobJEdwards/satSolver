module Problem (Problem(..)) where

class Problem a where
  solve :: a -> Maybe a
  example :: a

