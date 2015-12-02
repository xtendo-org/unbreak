module Run
    ( runInit
    , runOpen
    ) where

import Prelude hiding ((++))
import Control.Exception
import System.IO
import System.IO.Error
import System.Exit

import System.Posix.ByteString
import System.Process

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
        B.putStr "Type password: "
        password <- withNoEcho B.getLine
        let master = hash password (T.encodeUtf8 name)
        B.writeFile (B.unpack $ shelfPath ++ "/master") master
        createDirectory (shelfPath ++ "/file") 0o700
        return shelfPath

runOpen :: ByteString -> IO ()
runOpen filename = getConf f (`editRemoteFile` filename)
  where
    f errmsg = B.putStrLn ("Failed: " ++ errmsg) *> exitFailure

getConf :: (ByteString -> IO a) -> (Conf -> IO a) -> IO a
getConf failure success = do
    confPath <- (++ "/.unbreak.json") <$> getHomePath
    existence <- fileExist confPath
    if existence
    then do
        rawConf <- B.readFile (B.unpack confPath)
        case dec rawConf of
            Left errmsg -> failure $ B.pack errmsg
            Right conf -> success conf
    else do
        B.putStrLn "You may need to run unbreak (without arguments) first."
        failure "~/.unbreak.json does not exist"

editRemoteFile :: Conf -> ByteString -> IO ()
editRemoteFile conf@Conf{..} filename = do
    shelfPath <- session conf
    let
        filePath = mconcat [shelfPath, "/file/", filename]
        remoteFilePath = mconcat [T.encodeUtf8 remote, filename]
    -- copy the remote file to the shelf
    run (mconcat ["scp ", remoteFilePath, " ", filePath]) $
        \ n -> B.putStrLn $ mconcat
            ["Download failed. (", B.pack $ show n, ")\nOpening a new file."]
    -- edit the file in the shelf
    run (mconcat [T.encodeUtf8 editor, " ", filePath]) $ const $ do
        B.putStrLn "Editor exited abnormally. Editing cancelled."
        exitFailure
    -- upload the file from the shelf to the remote
    run (mconcat ["scp ", filePath, " ", remoteFilePath]) $
        \ n -> B.putStrLn $ mconcat
            ["Upload failed. (", B.pack $ show n, ")"]
    B.putStrLn "Done."

withNoEcho :: IO a -> IO a
withNoEcho action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin False) (hSetEcho stdin old) action

run :: ByteString -> (Int -> IO ()) -> IO ()
run cmd failHandler = do
    -- TODO: Avoid String <-> ByteString circus
    (_, _, _, p) <- createProcess (shell $ B.unpack cmd)
    c <- waitForProcess p
    case c of
        ExitFailure n -> failHandler n
        _ -> return ()
