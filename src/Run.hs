module Run
    ( runInit
    , runOpen
    ) where

import Prelude hiding ((++))
import Control.Exception
import System.IO
import System.IO.Error

import System.Posix.ByteString

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T

import Crypto.KDF.Scrypt

import Format

(++) :: Monoid m => m -> m -> m
(++) = mappend

sessionPath :: ByteString
sessionPath = "/tmp/unbreak.session"

getHomePath :: IO ByteString
getHomePath = getEnv "HOME" >>= \ m -> case m of
    Nothing -> error "$HOME not found"
    Just h -> return h

hash :: ByteString -> ByteString -> ByteString
hash = generate (Parameters 128 8 1 64)

runInit :: IO ()
runInit = do
    confPath <- (++ "/.unbreak.json") <$> getHomePath
    existence <- fileExist confPath
    if existence
    then B.putStrLn "There is already the ~/.unbreak.json file.\
        \ If you are willing to create the default config file,\
        \ please delete ~/.unbreak.json and retry.\n\
        \Warning: the \"name\" part of the config may be required to open\
        \ the documents you have created in the past."
    else do
        B.writeFile (B.unpack confPath) (enc initConf)
        B.putStrLn $ "Created the initial default configuration at " ++
            confPath

-- idempotent session
session :: Conf -> IO ByteString
session Conf{..} = catchIOError
    (B.readFile $ B.unpack sessionPath) $ -- get existing session
    const $ do -- or if that fails, create a new session
        shelfPath <- mkdtemp "/dev/shm/unbreak-"
        B.writeFile (B.unpack sessionPath) shelfPath
        B.putStrLn "Type password: "
        password <- withNoEcho B.getLine
        let master = hash password (T.encodeUtf8 name)
        B.writeFile (B.unpack $ shelfPath ++ "/master") master
        return shelfPath

runOpen :: Conf -> ByteString -> IO ()
runOpen conf@Conf{..} filename = do
    shelfPath <- session conf
    print shelfPath

withNoEcho :: IO a -> IO a
withNoEcho action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin False) (hSetEcho stdin old) action
