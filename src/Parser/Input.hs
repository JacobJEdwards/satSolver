{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Parser.Input (Input (..)) where
  
import Data.String (IsString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.List as List
  
class (Eq i, Monoid i, IsString i) => Input i where
  type Token i
  uncons :: i -> Maybe (Token i, i)
  head :: i -> Token i
  null :: i -> Bool
  unpack :: i -> String

instance Input BS.ByteString where
  type Token BS.ByteString = Char
  
  uncons :: BS.ByteString -> Maybe (Char, BS.ByteString)
  uncons = BS.uncons
  
  head :: BS.ByteString -> Char
  head = BS.head
  
  null :: BS.ByteString -> Bool
  null = BS.null
  
  unpack :: BS.ByteString -> String
  unpack = BS.unpack


instance Input Text where
  type Token Text = Char
  
  uncons :: Text -> Maybe (Char, Text)
  uncons = Text.uncons
  
  head :: Text -> Char
  head = Text.head
  
  null :: Text -> Bool
  null = Text.null
  
  unpack :: Text -> String
  unpack = Text.unpack

instance Input String where
  type Token String = Char
  
  uncons :: String -> Maybe (Char, String)
  uncons = List.uncons
  
  head :: String -> Char
  head = List.head
  
  null :: String -> Bool
  null = List.null
  
  unpack :: String -> String
  unpack = id

