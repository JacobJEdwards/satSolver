{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Expr
  ( Expr (..),
    Polarity (..),
    flipPolarity,
    toCNF,
    (.||.),
    (.&&.),
    (.!.),
  )
where
  

data Expr a where 
  Var :: (Eq a, Ord a, Show a) => a -> Expr a
  Not :: Expr a -> Expr a
  And :: Expr a -> Expr a -> Expr a
  Or :: Expr a -> Expr a -> Expr a
  Const :: Bool -> Expr a

deriving instance Eq a => Eq (Expr a)
deriving instance Ord a => Ord (Expr a)
  
infixr 3 .||.

(.||.) :: Expr a -> Expr a -> Expr a
(.||.) = Or

infixr 3 .&&.

(.&&.) :: Expr a -> Expr a -> Expr a
(.&&.) = And

infixr 3 .!.

(.!.) :: Expr a -> Expr a
(.!.) = Not

instance (Show a) => Show (Expr a) where
  show e = case e of
    Var c -> show c
    Not e' -> "¬" ++ show e'
    And e1 e2 -> showAnd e1 e2
    Or e1 e2 -> showOr e1 e2
    Const b -> show b
    where
      showDuo :: String -> Expr a -> Expr a -> String
      showDuo op e1 e2 = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
      
      showAnd :: Expr a -> Expr a -> String
      showAnd = showDuo "∧"
      
      showOr :: Expr a -> Expr a -> String
      showOr = showDuo "∨"

deMorgansLaws :: Expr a -> Expr a
deMorgansLaws (Not (Not e)) = deMorgansLaws e
deMorgansLaws (Not (And e1 e2)) = Or (deMorgansLaws $ Not e1) (deMorgansLaws $ Not e2)
deMorgansLaws (Not (Or e1 e2)) = And (deMorgansLaws $ Not e1) (deMorgansLaws $ Not e2)
deMorgansLaws (Not (Const b)) = Const $ not b
deMorgansLaws (And e1 e2) = And (deMorgansLaws e1) (deMorgansLaws e2)
deMorgansLaws (Or e1 e2) = Or (deMorgansLaws e1) (deMorgansLaws e2)
deMorgansLaws (Not e) = Not $ deMorgansLaws e
deMorgansLaws e = e

distributiveLaws :: Expr a -> Expr a
distributiveLaws (Or e1 (And e2 e3)) = And (Or (distributiveLaws e1) (distributiveLaws e2)) (Or (distributiveLaws e1) (distributiveLaws e3))
distributiveLaws (Or (And e1 e2) e3) = And (Or (distributiveLaws e3) (distributiveLaws e1)) (Or (distributiveLaws e3) (distributiveLaws e2))
distributiveLaws (Or e1 e2) = Or (distributiveLaws e1) (distributiveLaws e2)
distributiveLaws (And e1 e2) = And (distributiveLaws e1) (distributiveLaws e2)
distributiveLaws (Not e) = Not $ distributiveLaws e
distributiveLaws e = e

toCNF :: (Eq a) => Expr a -> Expr a
toCNF expr
  | expr == expr' = expr
  | otherwise = toCNF expr'
  where
    expr' = distributiveLaws $ deMorgansLaws expr

data Polarity = Positive | Negative | Mixed deriving (Show, Eq)

instance Semigroup Polarity where
  Positive <> Positive = Positive
  Negative <> Negative = Negative
  _ <> _ = Mixed

instance Monoid Polarity where
  mempty = Positive

flipPolarity :: Polarity -> Polarity
flipPolarity Positive = Negative
flipPolarity Negative = Positive
flipPolarity Mixed = Mixed
