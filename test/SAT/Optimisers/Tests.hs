{-# LANGUAGE ImportQualifiedPost #-}

module SAT.Optimisers.Tests (tests) where

import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import SAT qualified
import SAT.CNF qualified as SAT
import SAT.CNF.Tests ()
import SAT.Optimisers (collectLiterals, collectLiteralsToSet, eliminateLiterals)
import SAT.Polarity qualified as SAT
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck ((==>))
import Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Optimiser Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [collectLiteralsProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [test_collectLiterals]

collectLiteralsProps :: TestTree
collectLiteralsProps =
  testGroup
    "collectLiterals"
    [ QC.testProperty "collectLiterals" prop_collectLiterals,
      QC.testProperty "collectLiteralsToSet" prop_collectLiteralsToSet
    ]

prop_collectLiterals :: SAT.CNF -> Bool
prop_collectLiterals cnf = IntSet.fromList (collectLiterals cnf) == collectLiteralsToSet cnf

prop_collectLiteralsToSet :: SAT.CNF -> Bool
prop_collectLiteralsToSet cnf = IntSet.fromList (collectLiterals cnf) == collectLiteralsToSet cnf

-- unit tests
test_collectLiterals :: TestTree
test_collectLiterals =
  testGroup
    "collectLiterals"
    [ testCase "empty CNF" $
        collectLiterals (SAT.CNF []) @?= [],
      testCase "CNF with one clause" $
        collectLiterals (SAT.CNF [[1, 2, 3]]) @?= [1, 2, 3],
      testCase "CNF with multiple clauses" $
        collectLiterals (SAT.CNF [[1, 2], [2, 3], [3, 4]]) @?= [1, 2, 2, 3, 3, 4],
      testCase "CNF with negatives" $
        collectLiterals (SAT.CNF [[1, -2], [-2, -3], [3, 4]]) @?= [1, 2, 2, 3, 3, 4]
    ]

-- test_literalPolarities :: TestTree
-- test_literalPolarities = testGroup "literalPolarities"
--   [ testCase "empty CNF" $
--       literalPolarities (SAT.CNF []) @?= IntMap.empty
--   , testCase "CNF with one clause" $
--       literalPolarities (SAT.CNF [[1, 2, 3]]) @?= IntMap.fromList [(1, SAT.Positive), (2, SAT.Positive), (3, SAT.Positive)]
--   , testCase "CNF with multiple clauses" $
--       literalPolarities (SAT.CNF [[1, 2], [2, 3], [-3, 4]]) @?= IntMap.fromList [(1, SAT.Positive), (2, SAT.Positive), (3, SAT.Mixed), (4, SAT.Positive)]
--   , testCase "CNF with negatives" $
--       literalPolarities (SAT.CNF [[1, -2], [-2, -3], [3, 4]]) @?= IntMap.fromList [(1, SAT.Positive), (2, SAT.Negative), (3, SAT.Mixed), (4, SAT.Positive)]
--   ]

-- eliminateLiterals :: CNF -> Assignment -> DecisionLevel -> Assignment
-- test_eliminateLiterals :: TestTree
-- test_eliminateLiterals = testGroup "eliminateLiterals"
--   [ testCase "empty CNF" $
--         eliminateLiterals (SAT.CNF []) IntMap.empty 0 @?= IntMap.empty
--   ]
