{-# LANGUAGE DeriveGeneric #-}

module Unbreak.Format
    ( FromJSON(..)
    , dec
    , EncodeJSON(..)
    , Conf(..)
    , initConf
    ) where

import Data.Text
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Encode.Pretty

dec :: FromJSON a => ByteString -> Either String a
dec = eitherDecode . fromStrict

encBase :: ToJSON a => [Text] -> a -> ByteString
encBase keys = (toStrict .) $ encodePretty' defConfig
    { confIndent = Spaces 2
    , confCompare = keyOrder keys
    }

-- a separate class for explicit ordering of JSON fields
class EncodeJSON a where
    enc :: a -> ByteString

data Conf = Conf
    { name :: !Text
    , remote :: !Text
    , editor :: !Text
    }
    deriving (Show, Generic)

instance FromJSON Conf
instance ToJSON Conf
instance EncodeJSON Conf where
    enc = encBase ["name", "remote", "editor"]

initConf :: Conf
initConf = Conf
    "Your Name (or anything correctly memorable)"
    "username@example.com:path/to/docdir/"
    "vim"
