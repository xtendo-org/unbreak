module Data.ByteString.OverheadFree
    ( readFile
    , writeFile
    , module Data.ByteString.Char8
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Exception (bracket)
import System.IO (hClose)
import Data.ByteString.Char8 hiding (readFile, writeFile)
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
