{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SAT.DIMACS.CNF.Tests (tests) where

import Data.List
import Data.Set qualified as Set
import SAT.DIMACS.CNF
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck ((==>))
import Test.Tasty.QuickCheck as QC

-- type DIMACS :: Type
-- data DIMACS = DIMACS
--  { numVars :: Integer,
--    numClauses :: Integer,
--    clauses :: [Clause],
--    comments :: [Text]
--  }
--  deriving stock (Eq, Show)
--
instance Arbitrary DIMACS where
  arbitrary :: Gen DIMACS
  arbitrary = do
    clauses <- arbitrary
    let comments = [""]
    let numVars = genericLength $ Set.toList $ Set.fromList $ concat clauses
    let numClauses = genericLength clauses
    pure $ DIMACS {numVars, numClauses, clauses, comments}

tests :: TestTree
tests = testGroup "DIMACS CNF Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" []

unitTests :: TestTree
unitTests = testGroup "Unit tests" []
