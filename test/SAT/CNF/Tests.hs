{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}

module SAT.CNF.Tests (tests) where

import SAT.CNF
import Data.Set qualified as Set
import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck ((==>))
import Data.Set qualified as Set


instance Arbitrary CNF where
  arbitrary :: Gen CNF
  arbitrary =  CNF <$> arbitrary

tests :: TestTree
tests = testGroup "CNF Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" []

unitTests :: TestTree
unitTests = testGroup "Unit tests" []
