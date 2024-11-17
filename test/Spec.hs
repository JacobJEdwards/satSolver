{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where
  
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC
import Test.Tasty.QuickCheck ((==>))
import Test.Tasty.HUnit

import SAT.Polarity.Tests qualified as PolarityTests
import Options.Tests qualified as OptionsTests
import SAT.VSIDS.Tests qualified as VSIDSTests
  
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [PolarityTests.tests, OptionsTests.tests, VSIDSTests.tests]