{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

module SAT.VSIDS.Tests where

import SAT.VSIDS
import SAT.CNF (CNF(..), Clause)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck ((==>))

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap

tests :: TestTree
tests = testGroup "VISIDS Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [prop_decay]

prop_decay :: TestTree
-- decays all elements by decayFactor
prop_decay = QC.testProperty "decay" $ \vsids -> decay vsids == IntMap.map (* decayFactor) vsids

unitTests :: TestTree
unitTests = testGroup "Unit tests" [test_initVSIDS]

test_initVSIDS :: TestTree
test_initVSIDS = testCase "initVSIDS" $ do
  let cnf = CNF [[1, 2], [2, 3], [3, 4]]
  let vsids = initVSIDS cnf
  assertEqual "initVSIDS" (IntMap.fromList [(1, 1), (2, 2), (3, 2), (4, 1)]) vsids
  