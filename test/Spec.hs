{-# LANGUAGE InstanceSigs #-}

import Test.QuickCheck
import qualified SAT
import SAT(Expr(..))

instance Arbitrary SAT.Polarity where
  arbitrary :: Gen SAT.Polarity
  arbitrary = elements [SAT.Positive, SAT.Negative]


main :: IO ()
main = putStrLn "Test suite not yet implemented"
