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
  
type Error :: Type -> Type -> Type
data Error :: Type -> Type -> Type where
  EndOfInput :: Error i e
  Unexpected :: i -> Error i e
  CustomError :: e -> Error i e
  Empty :: Error i e

deriving stock instance (Eq i, Eq e) => Eq (Error i e)

instance (Show i, Show e) => Show (Error i e) where
  show :: Error i e -> String
  show = \case
    EndOfInput -> "End of input"
    Unexpected i -> "Unexpected " <> show i
    CustomError e -> "Error: " <> show e
    Empty -> "Empty"

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

instance (Monoid i, Monoid e) => Monoid (Error i e) where
  mempty :: Error i e
  mempty = Empty
  
  mappend :: Error i e -> Error i e -> Error i e
  mappend = (<>)