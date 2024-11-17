{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}

module SAT.Polarity.Tests (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC
import Test.Tasty.QuickCheck ((==>))
import Test.Tasty.HUnit 

import SAT.Polarity qualified as SAT

tests :: TestTree
tests = testGroup "Polarity Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [monoidProps, flipProps]

instance QC.Arbitrary SAT.Polarity where
  arbitrary :: QC.Gen SAT.Polarity
  arbitrary = QC.elements [SAT.Positive, SAT.Negative, SAT.Mixed]

test_monoid_flip :: TestTree
test_monoid_flip = testCase "monoid" $ 
  SAT.Positive <> SAT.Negative @?= SAT.Mixed

test_monoid_same :: TestTree
test_monoid_same = testCase "monoid" $ 
  SAT.Positive <> SAT.Positive @?= SAT.Positive
  
test_monoid_same' :: TestTree
test_monoid_same' = testCase "monoid" $
  SAT.Negative <> SAT.Negative @?= SAT.Negative
  
test_monoid_same'' :: TestTree
test_monoid_same'' = testCase "monoid" $
  SAT.Mixed <> SAT.Mixed @?= SAT.Mixed
  
test_monoid_same''' :: TestTree
test_monoid_same''' = testCase "monoid" $
  SAT.Mixed <> SAT.Positive @?= SAT.Mixed
  
test_monoid_same'''' :: TestTree
test_monoid_same'''' = testCase "monoid" $
  SAT.Mixed <> SAT.Negative @?= SAT.Mixed
  
unitTests :: TestTree
unitTests = testGroup "Unit tests" [test_monoid_flip, test_monoid_same, test_monoid_same', test_monoid_same'', test_monoid_same''', test_monoid_same'''']
  

flipProps :: TestTree
flipProps = testGroup "flipPolarity"
  [ QC.testProperty "flipPolarity" prop_flip
  ]
  
monoidProps :: TestTree
monoidProps = testGroup "Monoid"
  [ QC.testProperty "monoid" prop_monoid
  , QC.testProperty "monoid_neq" prop_monoid_neq
  , QC.testProperty "monoid_eq" prop_monoid_eq
  , QC.testProperty "monoid_eq'" prop_monoid_eq'
  , QC.testProperty "monoid_neq'" prop_monoid_neq'
  ]

prop_flip :: SAT.Polarity -> Bool
prop_flip p = SAT.flipPolarity (SAT.flipPolarity p) == p

prop_monoid :: SAT.Polarity -> SAT.Polarity -> Bool
prop_monoid a b = SAT.flipPolarity (a <> b) == SAT.flipPolarity a <> SAT.flipPolarity b

-- if not the same, should be mixed
prop_monoid_neq :: SAT.Polarity -> SAT.Polarity -> QC.Property
prop_monoid_neq a b = (a /= b) ==> a <> b == SAT.Mixed

-- if the same, should be the same
prop_monoid_eq :: SAT.Polarity -> Bool
prop_monoid_eq a = a <> a == a

-- if the same, should be the same
prop_monoid_eq' :: SAT.Polarity -> Bool
prop_monoid_eq' a = SAT.flipPolarity a <> SAT.flipPolarity a == SAT.flipPolarity a

-- if not the same, should be mixed
prop_monoid_neq' :: SAT.Polarity -> SAT.Polarity -> QC.Property
prop_monoid_neq' a b = (a /= b) ==> SAT.flipPolarity a <> SAT.flipPolarity b == SAT.Mixed
