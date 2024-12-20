{-# LANGUAGE ImportQualifiedPost #-}

module SAT.Solver.Tests where

import Data.List
import Data.Set qualified as Set
import SAT.Solver
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck ((==>))
import Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Solver Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" []

unitTests :: TestTree
unitTests = testGroup "Unit tests" []
