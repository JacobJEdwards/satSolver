{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module SAT.Expr
  ( type Expr (..),
    (.||.),
    (.&&.),
    (.!.),
    unVal,
    toList,
    ors,
    ands,
    toVar,
    type Solutions,
  )
where


import Data.Data (type Data)
import Data.Kind (type Type)
import Data.IntSet (type IntSet)

type Solutions = IntSet

type Expr :: Type -> Type
data Expr (a :: Type) where
  Var :: a -> Expr a
  Not :: Expr a -> Expr a
  And :: Expr a -> Expr a -> Expr a
  Or :: Expr a -> Expr a -> Expr a
  Val :: Bool -> Expr a

instance (Eq a) => Eq (Expr a) where
  {-# INLINEABLE (==) #-}
  {-# SPECIALIZE instance Eq (Expr Int) #-}
  (==) :: Expr a -> Expr a -> Bool
  Var v1 == Var v2 = v1 == v2
  Not e1 == Not e2 = e1 == e2
  And e1 e2 == And e1' e2' = e1 == e1' && e2 == e2'
  Or e1 e2 == Or e1' e2' = e1 == e1' && e2 == e2'
  Val b1 == Val b2 = b1 == b2
  _ == _ = False

deriving stock instance (Ord a) => Ord (Expr a)

deriving stock instance (Data a) => Data (Expr a)

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
{-# INLINE showDuo #-}

showAnd :: (Show a) => Expr a -> Expr a -> String
showAnd = showDuo "∧"
{-# INLINE showAnd #-}

showOr :: (Show a) => Expr a -> Expr a -> String
showOr = showDuo "∨"
{-# INLINE showOr #-}

-- look into show prec, migh tprevent the nested bracket thing
instance (Show a) => Show (Expr a) where
  {-# INLINEABLE show #-}
  show :: Expr a -> String
  show (Var v) = show v
  show (Not e) = "¬" ++ show e
  show (And e1 e2) = showAnd e1 e2
  show (Or e1 e2) = showOr e1 e2
  show (Val b) = show b

instance Semigroup (Expr a) where
  {-# INLINEABLE (<>) #-}
  (<>) :: Expr a -> Expr a -> Expr a
  (<>) = Or

instance Monoid (Expr a) where
  {-# INLINEABLE mempty #-}
  mempty :: Expr a
  mempty = Val False

instance Functor Expr where
  {-# INLINEABLE fmap #-}
  fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var v) = Var $ f v
  fmap f (Not e) = Not $ fmap f e
  fmap f (And e1 e2) = And (fmap f e1) (fmap f e2)
  fmap f (Or e1 e2) = Or (fmap f e1) (fmap f e2)
  fmap _ (Val b) = Val b

instance Foldable Expr where
  {-# INLINEABLE foldMap #-}
  foldMap :: (Monoid m) => (a -> m) -> Expr a -> m
  foldMap f (Var v) = f v
  foldMap f (Not e) = foldMap f e
  foldMap f (And e1 e2) = foldMap f e1 <> foldMap f e2
  foldMap f (Or e1 e2) = foldMap f e1 <> foldMap f e2
  foldMap _ (Val _) = mempty

instance Traversable Expr where
  {-# INLINEABLE traverse #-}
  traverse :: (Applicative f) => (a -> f b) -> Expr a -> f (Expr b)
  traverse f (Var v) = Var <$> f v
  traverse f (Not e) = Not <$> traverse f e
  traverse f (And e1 e2) = And <$> traverse f e1 <*> traverse f e2
  traverse f (Or e1 e2) = Or <$> traverse f e1 <*> traverse f e2
  traverse _ (Val b) = pure $ Val b

instance Applicative Expr where
  {-# INLINEABLE pure #-}
  {-# INLINEABLE (<*>) #-}
  pure :: a -> Expr a
  pure = Var

  (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  (<*>) (Var f) e = f <$> e
  (<*>) (Not f) e = Not (f <*> e)
  (<*>) (And f1 f2) e = And (f1 <*> e) (f2 <*> e)
  (<*>) (Or f1 f2) e = Or (f1 <*> e) (f2 <*> e)
  (<*>) (Val b) _ = Val b

instance Monad Expr where
  (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (>>=) (Var v) f = f v
  (>>=) (Not e) f = Not (e >>= f)
  (>>=) (And e1 e2) f = And (e1 >>= f) (e2 >>= f)
  (>>=) (Or e1 e2) f = Or (e1 >>= f) (e2 >>= f)
  (>>=) (Val b) _ = Val b


-- propagate error to caller with maybe or leave as is ?
unVal :: Expr a -> Bool
unVal (Val b) = b
unVal _ = error "Not a constant"
{-# INLINEABLE unVal #-}

toList :: Expr a -> [Expr a]
toList x =
  x : case x of
    Var _ -> []
    Not e -> toList e
    And e1 e2 -> toList e1 ++ toList e2
    Or e1 e2 -> toList e1 ++ toList e2
    Val _ -> []
{-# INLINEABLE toList #-}

ors :: [Expr a] -> Expr a
ors = foldr1 Or
{-# INLINEABLE ors #-}

ands :: [Expr a] -> Expr a
ands = foldr1 And
{-# INLINEABLE ands #-}

toVar :: Int -> Expr Int
toVar n
  | n < 0 = Not $ Var $ abs n
  | otherwise = Var n
{-# INLINEABLE toVar #-}
