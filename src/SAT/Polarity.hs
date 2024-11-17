{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module SAT.Polarity (type Polarity (Positive, Negative, Mixed), flipPolarity) where
  
import Data.Kind (type Type)

type Polarity :: Type
data Polarity = Positive | Negative | Mixed deriving stock (Eq, Ord, Show)

instance Semigroup Polarity where
  (<>) :: Polarity -> Polarity -> Polarity
  (<>) Positive Positive = Positive
  (<>) Negative Negative = Negative
  (<>) _ _ = Mixed

-- does this make sense ? check laws
instance Monoid Polarity where
  mempty :: Polarity
  mempty = Positive

flipPolarity :: Polarity -> Polarity
flipPolarity Positive = Negative
flipPolarity Negative = Positive
flipPolarity Mixed = Mixed
{-# INLINEABLE flipPolarity #-}
