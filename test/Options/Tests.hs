{-# LANGUAGE OverloadedStrings #-}

module Options.Tests (tests) where
  
import Options (Flag (..), parseArgs)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "Options" [testParseArgs]

-- TODO
testParseArgs :: TestTree
testParseArgs = testCase "parseArgs" $ do 
  result <- parseArgs ["-i"]
  result @?= Interactive
  
  

