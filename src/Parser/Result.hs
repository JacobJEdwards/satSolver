{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.Result (Result(..)) where
  
import Parser.Error
import Data.List (intercalate)
import Control.Applicative (Alternative(..))
  
data Result i e a = Errors [Error i e] | Result a

instance (Show a, Show e, Show i) => Show (Result i e a) where
  show :: Result i e a -> String
  show (Result res) = show res
  show (Errors errs) = "Errors: " <> intercalate ", " (show <$> errs)

instance Functor (Result i e) where
  fmap :: (a -> b) -> Result i e a -> Result i e b
  fmap _ (Errors errs) = Errors errs
  fmap f (Result res) = Result $ f res

instance Applicative (Result i e) where
  pure :: a -> Result i e a
  pure = Result

  (<*>) :: Result i e (a -> b) -> Result i e a -> Result i e b
  (<*>) (Errors errs) _ = Errors errs
  (<*>) _ (Errors errs) = Errors errs
  (<*>) (Result f) (Result x) = Result $ f x

instance Alternative (Result i e) where
  empty :: Result i e a
  empty = Errors [Empty]

  (<|>) :: Result i e a -> Result i e a ->Result i e a
  (<|>) (Errors errs) (Errors errs') = Errors $ errs <> errs'
  (<|>) l@(Result _) _ = l
  (<|>) _ r@(Result _) = r

instance (Semigroup a) => Semigroup (Result i e a) where
  (<>) :: Result i e a -> Result i e a -> Result i e a
  (<>) (Result x) (Result y) = Result $ x <> y
  (<>) (Errors errs1) (Errors errs2) = Errors $ errs1 <> errs2
  (<>) (Errors errs) _ = Errors errs
  (<>) _ (Errors errs) = Errors errs

instance (Monoid a) => Monoid (Result i e a) where
  mempty :: Result i e a
  mempty = Result mempty

instance Monad (Result i e) where
  return :: a -> Result i e a
  return = pure

  (>>=) :: Result i e a -> (a -> Result i e b) -> Result i e b
  (>>=) (Errors errs) _ = Errors errs
  (>>=) (Result res) f = f res

