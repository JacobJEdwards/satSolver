{-|
Module      : SAT.Polarity
Description : Exports the Polarity module.
-}

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

-- | The 'Polarity' type.
type Polarity :: Type
data Polarity = Positive | Negative | Mixed deriving stock (Eq, Ord, Show)

-- | Semigroup instance for the 'Polarity' type.
instance Semigroup Polarity where
  -- | Combines two polarities.
  -- 
  -- >>> Positive <> Positive
  -- Positive
  -- 
  -- >>> Negative <> Negative
  -- Negative
  -- 
  -- >>> Positive <> Negative
  -- Mixed
  -- 
  -- >>> Positive <> Mixed
  -- Mixed
  -- 
  -- >>> Negative <> Mixed
  -- Mixed
  -- 
  -- prop> \x y -> x <> y == y <> x
  -- 
  -- prop> \x -> x <> Mixed == Mixed
  (<>) :: Polarity -> Polarity -> Polarity
  (<>) Positive Positive = Positive
  (<>) Negative Negative = Negative
  (<>) _ _ = Mixed

-- | Monoid instance for the 'Polarity' type.
instance Monoid Polarity where
  -- | The identity element of the 'Polarity' type.
  mempty :: Polarity
  mempty = Positive

-- | Flips the polarity.
-- 
-- >>> flipPolarity Positive
-- Negative
-- 
-- >>> flipPolarity Negative
-- Positive
-- 
-- >>> flipPolarity Mixed
-- Mixed
-- 
-- prop> flipPolarity (flipPolarity x) == x
flipPolarity :: Polarity -> Polarity
flipPolarity Positive = Negative
flipPolarity Negative = Positive
flipPolarity Mixed = Mixed
{-# INLINEABLE flipPolarity #-}
