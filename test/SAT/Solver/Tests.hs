module SAT.Solver.Tests where

import SAT.Solver
import Data.Set qualified as Set
import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck ((==>))

tests :: TestTree
tests = testGroup "Solver Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" []

unitTests :: TestTree
unitTests = testGroup "Unit tests" []