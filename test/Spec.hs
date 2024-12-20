{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Options.Tests qualified as OptionsTests
import SAT.Polarity.Tests qualified as PolarityTests
import SAT.VSIDS.Tests qualified as VSIDSTests
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck ((==>))
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [PolarityTests.tests, OptionsTests.tests, VSIDSTests.tests]
