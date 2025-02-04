
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Strict #-}

-- |
-- Module      : SAT.Expr
-- Description : Exports the SAT expression module.
module SAT.Expr.Internal
  ( type Expr (..),
    (.||.),
    (.&&.),
    (.!.),
    unVal,
    toList,
    ors,
    ands,
    toVar,
  )
where

import Control.Parallel.Strategies (type NFData)
import Data.Data (type Data)
import Data.Kind (type Type)
import GHC.Generics (type Generic)


-- | The 'Expr' type.
type Expr :: Type -> Type
data Expr (a :: Type) where
  Var :: a -> Expr a
  Not :: Expr a -> Expr a
  And :: Expr a -> Expr a -> Expr a
  Or :: Expr a -> Expr a -> Expr a
  Val :: Bool -> Expr a
  Implies :: Expr a -> Expr a -> Expr a
  XOr :: Expr a -> Expr a -> Expr a
  XNor :: Expr a -> Expr a -> Expr a
  NAnd :: Expr a -> Expr a -> Expr a
  NOr :: Expr a -> Expr a -> Expr a

-- | Eq instance for the 'Expr' type.
-- Defined over derived in order to specialize it.
instance (Eq a) => Eq (Expr a) where
  {-# INLINEABLE (==) #-}
  {-# SPECIALIZE instance Eq (Expr Int) #-}
  (==) :: Expr a -> Expr a -> Bool
  Var v1 == Var v2 = v1 == v2
  Not e1 == Not e2 = e1 == e2
  And e1 e2 == And e1' e2' = e1 == e1' && e2 == e2'
  Or e1 e2 == Or e1' e2' = e1 == e1' && e2 == e2'
  Val b1 == Val b2 = b1 == b2
  Implies e1 e2 == Implies e1' e2' = e1 == e1' && e2 == e2'
  XOr e1 e2 == XOr e1' e2' = e1 == e1' && e2 == e2'
  XNor e1 e2 == XNor e1' e2' = e1 == e1' && e2 == e2'
  NAnd e1 e2 == NAnd e1' e2' = e1 == e1' && e2 == e2'
  NOr e1 e2 == NOr e1' e2' = e1 == e1' && e2 == e2'
  _ == _ = False

deriving stock instance (Ord a) => Ord (Expr a)

deriving stock instance (Data a) => Data (Expr a)

deriving stock instance (Read a) => Read (Expr a)

deriving stock instance Functor Expr

deriving stock instance Foldable Expr

deriving stock instance Traversable Expr

deriving stock instance Generic (Expr a)

deriving anyclass instance (NFData a) => NFData (Expr a)

(.||.) :: Expr a -> Expr a -> Expr a
(.||.) = Or

infixr 3 .||.

(.&&.) :: Expr a -> Expr a -> Expr a
(.&&.) = And

infixr 3 .&&.

(.!.) :: Expr a -> Expr a
(.!.) = Not

infixr 3 .!.

-- | Shows a pair of expressions.
showDuo :: (Show a) => String -> Expr a -> Expr a -> String
showDuo op e1 e2 = "(" <> show e1 <> " " <> op <> " " <> show e2 ++ ")"
{-# INLINE showDuo #-}

-- | Shows an 'And' expression.
showAnd :: (Show a) => Expr a -> Expr a -> String
showAnd = showDuo "∧"
{-# INLINE showAnd #-}

-- | Shows an 'Or' expression.
showOr :: (Show a) => Expr a -> Expr a -> String
showOr = showDuo "∨"
{-# INLINE showOr #-}

-- look into show prec, migh tprevent the nested bracket thing

-- | Show instance for the 'Expr' type.
instance (Show a) => Show (Expr a) where
  {-# INLINEABLE show #-}
  show :: Expr a -> String
  show (Var v) = show v
  show (Not e) = "¬" <> show e
  show (And e1 e2) = showAnd e1 e2
  show (Or e1 e2) = showOr e1 e2
  show (Val b) = show b
  show (Implies e1 e2) = showDuo "=>" e1 e2
  show (XOr e1 e2) = showDuo "⊕" e1 e2
  show (XNor e1 e2) = showDuo "⊙" e1 e2
  show (NAnd e1 e2) = showDuo "↑" e1 e2
  show (NOr e1 e2) = showDuo "↓" e1 e2

-- | Semigroup instance for the 'Expr' type.
--
-- >>> Var 1 <> Var 2
-- Or (Var 1) (Var 2)
instance Semigroup (Expr a) where
  {-# INLINEABLE (<>) #-}
  (<>) :: Expr a -> Expr a -> Expr a
  (<>) = Or

-- | Monoid instance for the 'Expr' type.
--
-- >>> mempty :: Expr Int
-- Val False
instance Monoid (Expr a) where
  {-# INLINEABLE mempty #-}
  mempty :: Expr a
  mempty = Val False

-- | Applicative instance for the 'Expr' type.
--
-- >>> pure 1 :: Expr Int
-- Var 1
instance Applicative Expr where
  {-# INLINEABLE pure #-}
  {-# INLINEABLE (<*>) #-}

  -- \| Lifts a value to an expression.
  pure :: a -> Expr a
  pure = Var

  -- \| Applies a function in an expression to a value in an expression.
  (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  (<*>) (Var f) e = f <$> e
  (<*>) (Not f) e = Not $ f <*> e
  (<*>) (And f1 f2) e = And (f1 <*> e) (f2 <*> e)
  (<*>) (Or f1 f2) e = Or (f1 <*> e) (f2 <*> e)
  (<*>) (Implies f1 f2) e = Implies (f1 <*> e) (f2 <*> e)
  (<*>) (Val b) _ = Val b
  (<*>) (XOr f1 f2) e = XOr (f1 <*> e) (f2 <*> e)
  (<*>) (XNor f1 f2) e = XNor (f1 <*> e) (f2 <*> e)
  (<*>) (NAnd f1 f2) e = NAnd (f1 <*> e) (f2 <*> e)
  (<*>) (NOr f1 f2) e = NOr (f1 <*> e) (f2 <*> e)

-- | Monad instance for the 'Expr' type.
--
-- >>> Var 1 >>= \x -> Var (x + 1)
-- Var 2
instance Monad Expr where
  {-# INLINEABLE (>>=) #-}
  -- \| Binds a value in an expression to a function.
  (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (>>=) (Var v) f = f v
  (>>=) (Not e) f = Not $ e >>= f
  (>>=) (And e1 e2) f = And (e1 >>= f) (e2 >>= f)
  (>>=) (Or e1 e2) f = Or (e1 >>= f) (e2 >>= f)
  (>>=) (Implies e1 e2) f = Implies (e1 >>= f) (e2 >>= f)
  (>>=) (Val b) _ = Val b
  (>>=) (XOr e1 e2) f = XOr (e1 >>= f) (e2 >>= f)
  (>>=) (XNor e1 e2) f = XNor (e1 >>= f) (e2 >>= f)
  (>>=) (NAnd e1 e2) f = NAnd (e1 >>= f) (e2 >>= f)
  (>>=) (NOr e1 e2) f = NOr (e1 >>= f) (e2 >>= f)

-- propagate error to caller with maybe or leave as is ?

-- | Gets the value of a constant expression.
unVal :: Expr a -> Bool
unVal (Val b) = b
unVal _ = error "Not a constant"
{-# INLINEABLE unVal #-}

-- | Converts an expression to a list.
--
-- >>> toList (Var 1 .&&. Var 2)
-- [1, 2]
toList :: Expr a -> [a]
toList = foldr (:) []
{-# INLINEABLE toList #-}

-- | Combines a list of expressions with 'Or'.
--
-- >>> ors [Var 1, Var 2]
-- Or (Var 1) (Var 2)
ors :: [Expr a] -> Expr a
ors = foldr1 Or
{-# INLINEABLE ors #-}

-- | Combines a list of expressions with 'And'.
--
-- >>> ands [Var 1, Var 2]
-- And (Var 1) (Var 2)
ands :: [Expr a] -> Expr a
ands = foldr1 And
{-# INLINEABLE ands #-}

-- | Converts a literal to a variable.
--
-- >>> toVar 1
-- Var 1
--
-- >>> toVar (-1)
-- Not (Var 1)
toVar :: Int -> Expr Int
toVar n
  | n < 0 = Not $ Var $ abs n
  | otherwise = Var n
{-# INLINEABLE toVar #-}