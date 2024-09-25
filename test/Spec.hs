{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Test.QuickCheck
import SAT qualified
import SAT(Expr(..))

instance Arbitrary SAT.Polarity where
  arbitrary :: Gen SAT.Polarity
  arbitrary = elements [SAT.Positive, SAT.Negative, SAT.Mixed]

main :: IO ()
main = putStrLn "Test suite not yet implemented"
