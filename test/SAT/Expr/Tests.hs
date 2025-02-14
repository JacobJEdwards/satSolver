{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}

module SAT.Expr.Tests where

import Data.List
import Data.Set qualified as Set
import SAT.Expr
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck ((==>))
import Test.Tasty.QuickCheck as QC

instance Arbitrary (Expr Int) where
  arbitrary :: Gen (Expr Int)
  arbitrary = sized go
    where
      go :: Int -> Gen (Expr Int)
      go 0 = do
        Var <$> arbitrary
      go n = do
        isNot <- arbitrary
        if isNot
          then Not <$> go (n - 1)
          else do
            op <- elements [And, Or]
            m <- choose (0, n - 1)
            l <- go m
            r <- go (n - 1 - m)
            return $ op l r

tests :: TestTree
tests = testGroup "Expr Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" []

unitTests :: TestTree
unitTests = testGroup "Unit tests" []
