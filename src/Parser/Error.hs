{-|
Module: Parser.Error
Description: Defines the error type for the parser.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Parser.Error (type Error(..)) where
  
import Data.Kind (Type)
  
-- | The error type for the parser.
type Error :: Type -> Type -> Type
data Error :: Type -> Type -> Type where
  -- | The input ended unexpectedly.
  EndOfInput :: Error i e
  -- | An unexpected token was encountered.
  Unexpected :: i -> Error i e
  -- | A custom error occurred.
  CustomError :: e -> Error i e
  -- | The input is empty.
  Empty :: Error i e

deriving stock instance (Eq i, Eq e) => Eq (Error i e)

-- | Shows the error.
instance (Show i, Show e) => Show (Error i e) where
  show :: Error i e -> String
  show = \case
    EndOfInput -> "End of input"
    Unexpected i -> "Unexpected " <> show i
    CustomError e -> "Error: " <> show e
    Empty -> "Empty"

-- | Semigroup instance for the error type.
instance (Semigroup i, Semigroup e) => Semigroup (Error i e) where
  (<>) :: Error i e -> Error i e -> Error i e
  (<>) = \case
    EndOfInput -> \case
      EndOfInput -> EndOfInput
      Unexpected i -> Unexpected i
      CustomError e -> CustomError e
      Empty -> Empty
    Unexpected i -> \case
      EndOfInput -> Unexpected i
      Unexpected i' -> Unexpected $ i <> i'
      CustomError e -> CustomError e
      Empty -> Empty
    CustomError e -> \case
      EndOfInput -> CustomError e
      Unexpected _ -> CustomError e
      CustomError e' -> CustomError $ e <> e'
      Empty -> Empty
    Empty -> \case
      EndOfInput -> Empty
      Unexpected _ -> Empty
      CustomError _ -> Empty
      Empty -> Empty

-- | Monoid instance for the error type.
instance (Monoid i, Monoid e) => Monoid (Error i e) where
  mempty :: Error i e
  mempty = Empty
  
  mappend :: Error i e -> Error i e -> Error i e
  mappend = (<>)
  