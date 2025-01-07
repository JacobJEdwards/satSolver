{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SAT.Encode (type Encodable (encode, decode, type Code)) where

import Data.Char (chr, ord)
import Data.Kind (type Type)
import Data.List (foldl')

class Encodable (a :: Type) where
  type Code a :: Type

  encode :: a -> Code a
  decode :: Code a -> a

instance Encodable String where
  type Code String = Int

  encode :: String -> Int
  encode = foldl' (\acc c -> acc * 256 + ord c) 0

  decode :: Int -> String
  decode 0 = ""
  decode n = decode (n `div` 256) ++ [chr (n `mod` 256)]

instance Encodable Int where
  type Code Int = Int

  encode :: Int -> Int
  encode = id

  decode :: Int -> Int
  decode = id

instance Encodable Bool where
  type Code Bool = Int

  encode :: Bool -> Int
  encode True = 1
  encode False = 0

  decode :: Int -> Bool
  decode 0 = False
  decode _ = True

instance Encodable Char where
  type Code Char = Int

  encode :: Char -> Int
  encode = ord

  decode :: Int -> Char
  decode = chr
