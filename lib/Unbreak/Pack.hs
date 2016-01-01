module Unbreak.Pack
    ( str
    , int
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as B

str :: String -> ByteString
str = LB.toStrict . B.toLazyByteString . B.stringUtf8

int :: Int -> ByteString
int = LB.toStrict . B.toLazyByteString . B.intDec
