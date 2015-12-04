module Data.ByteString.IO
    ( readFile
    , writeFile
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Exception (bracket)
import System.IO (hClose)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.Posix.ByteString

defaultFlags :: OpenFileFlags
defaultFlags = OpenFileFlags
    { append = False
    , exclusive = False
    , noctty = True
    , nonBlock = False
    , trunc = False
    }

readFile :: RawFilePath -> IO ByteString
readFile path = bracket open hClose B.hGetContents
  where
    open = openFd path ReadOnly Nothing defaultFlags >>= fdToHandle

writeFile :: RawFilePath -> ByteString -> IO ()
writeFile path content = bracket open hClose (`B.hPut` content)
  where
    open = createFile path regularFileMode >>= fdToHandle
