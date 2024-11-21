{-|
Module      : Parser.Input
Description : Defines the 'Input' type class for parsing input types.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Parser.Input (type Input (..)) where

import Data.ByteString.Char8 qualified as BS
import Data.Kind (type Constraint, type Type)
import Data.List qualified as List
import Data.String (type IsString)
import Data.Text (type Text)
import Data.Text qualified as Text

-- | The 'Input' type class for parsing
type Input :: Type -> Constraint
class (Eq i, IsString i, Monoid i) => Input (i :: Type) where
  -- | The token type of the input.
  type Token i
  -- | Uncons the input.
  uncons :: i -> Maybe (Token i, i)
  -- | Get the head of the input.
  head :: i -> Maybe (Token i)
  -- | Check if the input is empty.
  null :: i -> Bool
  -- | Unpack the input to a string.
  unpack :: i -> String

-- | Instances of the 'Input' type class for different input types.

-- | 'Input' instance for 'BS.ByteString'.
instance Input BS.ByteString where
  type Token BS.ByteString = Char

  uncons :: BS.ByteString -> Maybe (Char, BS.ByteString)
  uncons = BS.uncons
  {-# INLINEABLE uncons #-}

  head :: BS.ByteString -> Maybe Char
  head = fmap fst . BS.uncons
  {-# INLINEABLE head #-}

  null :: BS.ByteString -> Bool
  null = BS.null
  {-# INLINEABLE null #-}

  unpack :: BS.ByteString -> String
  unpack = BS.unpack
  {-# INLINEABLE unpack #-}

-- | 'Input' instance for 'Text'.
instance Input Text where
  type Token Text = Char

  uncons :: Text -> Maybe (Char, Text)
  uncons = Text.uncons
  {-# INLINEABLE uncons #-}

  head :: Text -> Maybe Char
  head t = fst <$> Text.uncons t
  {-# INLINEABLE head #-}

  null :: Text -> Bool
  null = Text.null
  {-# INLINEABLE null #-}

  unpack :: Text -> String
  unpack = Text.unpack
  {-# INLINEABLE unpack #-}

-- | 'Input' instance for 'String'.
instance Input String where
  type Token String = Char

  uncons :: String -> Maybe (Char, String)
  uncons = List.uncons
  {-# INLINEABLE uncons #-}

  head :: String -> Maybe Char
  head t = fst <$> List.uncons t
  {-# INLINEABLE head #-}

  null :: String -> Bool
  null = List.null
  {-# INLINEABLE null #-}

  unpack :: String -> String
  unpack = id
  {-# INLINEABLE unpack #-}
