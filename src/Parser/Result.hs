{-|
Module: Parser.Result
Description: Exports the Result type.
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Parser.Result (type Result (..)) where

import Control.Applicative (type Alternative (empty, (<|>)))
import Data.Kind (type Type)
import Data.List (intercalate)
import Parser.Error (type Error (type Empty))

-- | The result type for the parser.
type Result :: Type -> Type -> Type -> Type
data Result i e a = Errors [Error i e] | Result a

-- | Shows the result.
instance (Show a, Show e, Show i) => Show (Result i e a) where
  show :: Result i e a -> String
  show (Result res) = show res
  show (Errors errs) = "Errors: " <> intercalate ", " (show <$> errs)

-- | Functor instance for the result type.
instance Functor (Result i e) where
  -- | Maps a function over the result.
  fmap :: (a -> b) -> Result i e a -> Result i e b
  fmap _ (Errors errs) = Errors errs
  fmap f (Result res) = Result $ f res

-- | Applicative instance for the result type.
instance Applicative (Result i e) where
  -- | Lifts a value into the result.
  pure :: a -> Result i e a
  pure = Result

  -- | Applies a function in the result to a value in the result.
  (<*>) :: Result i e (a -> b) -> Result i e a -> Result i e b
  (<*>) (Errors errs) _ = Errors errs
  (<*>) _ (Errors errs) = Errors errs
  (<*>) (Result f) (Result x) = Result $ f x

-- | Alternative instance for the result type.
instance Alternative (Result i e) where
  -- | The empty result.
  empty :: Result i e a
  empty = Errors [Empty]

  -- | Choice of two results.
  (<|>) :: Result i e a -> Result i e a -> Result i e a
  (<|>) (Errors errs) (Errors errs') = Errors $ errs <> errs'
  (<|>) l@(Result _) _ = l
  (<|>) _ r@(Result _) = r

-- | Semigroup instance for the result type.
instance (Semigroup a) => Semigroup (Result i e a) where
  -- | Combines two results.
  (<>) :: Result i e a -> Result i e a -> Result i e a
  (<>) (Result x) (Result y) = Result $ x <> y
  (<>) (Errors errs1) (Errors errs2) = Errors $ errs1 <> errs2
  (<>) (Errors errs) _ = Errors errs
  (<>) _ (Errors errs) = Errors errs

-- | Monoid instance for the result type.
instance (Monoid a) => Monoid (Result i e a) where
  -- | The empty result.
  mempty :: Result i e a
  mempty = Result mempty

-- | Monad instance for the result type.
instance Monad (Result i e) where
  -- | Returns a value in the result.
  return :: a -> Result i e a
  return = pure

  -- | Binds a function to the result.
  (>>=) :: Result i e a -> (a -> Result i e b) -> Result i e b
  (>>=) (Errors errs) _ = Errors errs
  (>>=) (Result res) f = f res
