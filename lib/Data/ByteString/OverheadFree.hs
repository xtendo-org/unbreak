-- |
-- Module       : Data.ByteString.OverheadFree
-- License      : AGPL-3
-- Maintainer   : Kinoru
-- Stability    : Temporary
-- Portability  : POSIX
--
-- This module is intended to be a drop-in replacement of
-- "Data.ByteString.Char8".
--
-- The only two functions whose type signatures have changed are 'readFile'
-- and 'writeFile'. They take 'RawFilePath' instead of 'FilePath'. This is
-- to avoid the unnecessary 'String' - 'ByteString' conversion overhead.
--
-- The ongoing
-- <https://ghc.haskell.org/trac/ghc/wiki/Proposal/AbstractFilePath Abstract FilePath Proposal>
-- should address this issue, so this is a
-- temporary resolution until the proposal is merged.
--
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

-- | Read an entire file at the 'RawFilePath' strictly into a 'ByteString'.
readFile :: RawFilePath -> IO ByteString
readFile path = bracket open hClose B.hGetContents
  where
    open = openFd path ReadOnly Nothing defaultFlags >>= fdToHandle

-- | Write a 'ByteString' to a file at the 'RawFilePath'.
writeFile :: RawFilePath -> ByteString -> IO ()
writeFile path content = bracket open hClose (`B.hPut` content)
  where
    open = createFile path dfm >>= fdToHandle
    dfm = unionFileModes ownerReadMode ownerWriteMode
