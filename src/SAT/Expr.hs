{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module SAT.Expr
  ( Expr (..),
    Polarity (..),
    flipPolarity,
    (.||.),
    (.&&.),
    (.!.),
    unVal,
    toList,
    ors,
    ands,
    toVar
  )
where

import Control.Applicative (Alternative(..))
import Data.Data (Data, Typeable)

data Expr a where
  Var :: a -> Expr a
  Not :: Expr a -> Expr a
  And :: Expr a -> Expr a -> Expr a
  Or :: Expr a -> Expr a -> Expr a
  Val :: Bool -> Expr a

deriving instance Eq a => Eq (Expr a)
deriving instance Ord a => Ord (Expr a)
deriving instance Data a => Data (Expr a)
deriving instance Typeable a => Typeable (Expr a)

infixr 3 .||.

(.||.) :: Expr a -> Expr a -> Expr a
(.||.) = Or

infixr 3 .&&.

(.&&.) :: Expr a -> Expr a -> Expr a
(.&&.) = And

infixr 3 .!.

(.!.) :: Expr a -> Expr a
(.!.) = Not

showDuo :: (Show a) => String -> Expr a -> Expr a -> String
showDuo op e1 e2 = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"

showAnd :: (Show a) => Expr a -> Expr a -> String
showAnd = showDuo "∧"

showOr :: (Show a) => Expr a -> Expr a -> String
showOr = showDuo "∨"

instance (Show a) => Show (Expr a) where
  show :: Expr a -> String
  show (Var v) = show v
  show (Not e) = "¬" ++ show e
  show (And e1 e2) = showAnd e1 e2
  show (Or e1 e2) = showOr e1 e2
  show (Val b) = show b


instance Semigroup (Expr a) where
  (<>) :: Expr a -> Expr a -> Expr a
  (<>) = Or

instance Monoid (Expr a) where
  mempty :: Expr a
  mempty = Val False

instance Functor Expr where
  fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var v) = Var $ f v
  fmap f (Not e) = Not $ fmap f e
  fmap f (And e1 e2) = And (fmap f e1) (fmap f e2)
  fmap f (Or e1 e2) = Or (fmap f e1) (fmap f e2)
  fmap _ (Val b) = Val b

instance Foldable Expr where
  foldMap :: Monoid m => (a -> m) -> Expr a -> m
  foldMap f (Var v) = f v
  foldMap f (Not e) = foldMap f e
  foldMap f (And e1 e2) = foldMap f e1 <> foldMap f e2
  foldMap f (Or e1 e2) = foldMap f e1 <> foldMap f e2
  foldMap _ (Val _) = mempty

instance Traversable Expr where
  traverse :: Applicative f => (a -> f b) -> Expr a -> f (Expr b)
  traverse f (Var v) = Var <$> f v
  traverse f (Not e) = Not <$> traverse f e
  traverse f (And e1 e2) = And <$> traverse f e1 <*> traverse f e2
  traverse f (Or e1 e2) = Or <$> traverse f e1 <*> traverse f e2
  traverse _ (Val b) = pure $ Val b

instance Applicative Expr where
  pure :: a -> Expr a
  pure = Var

  (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  Var f <*> e = f <$> e
  Not f <*> e = Not (f <*> e)
  And f1 f2 <*> e = And (f1 <*> e) (f2 <*> e)
  Or f1 f2 <*> e = Or (f1 <*> e) (f2 <*> e)
  Val b <*> _ = Val b

instance Alternative Expr where
  empty :: Expr a
  empty = Val False

  (<|>) :: Expr a -> Expr a -> Expr a
  (<|>) = Or

instance Monad Expr where
  (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Var v >>= f = f v
  Not e >>= f = Not (e >>= f)
  And e1 e2 >>= f = And (e1 >>= f) (e2 >>= f)
  Or e1 e2 >>= f = Or (e1 >>= f) (e2 >>= f)
  Val b >>= _ = Val b

instance MonadFail Expr where
  fail :: String -> Expr a
  fail = error

data Polarity = Positive | Negative | Mixed deriving (Eq, Ord, Show)

instance Semigroup Polarity where
  (<>) :: Polarity -> Polarity -> Polarity
  (<>) Positive Positive = Positive
  (<>) Negative Negative = Negative
  (<>) _ _ = Mixed

instance Monoid Polarity where
  mempty :: Polarity
  mempty = Positive
  
flipPolarity :: Polarity -> Polarity
flipPolarity Positive = Negative
flipPolarity Negative = Positive
flipPolarity Mixed = Mixed

unVal :: Expr a -> Bool
unVal (Val b) = b
unVal _ = error "Not a constant"

toList :: Expr a -> [Expr a]
toList x = x : case x of
  Var _ -> []
  Not e -> toList e
  And e1 e2 -> toList e1 ++ toList e2
  Or e1 e2 -> toList e1 ++ toList e2
  Val _ -> []

ors :: [Expr a] -> Expr a
ors = foldr1 Or

ands :: [Expr a] -> Expr a
ands = foldr1 And

toVar :: Int -> Expr Int
toVar n = if n < 0 then Not $ Var $ abs n else Var n
