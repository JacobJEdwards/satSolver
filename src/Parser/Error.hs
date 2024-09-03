{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module Parser.Error (Error(..)) where
  
data Error i e = EndOfInput | Unexpected i | CustomError e | Empty deriving (Eq)

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