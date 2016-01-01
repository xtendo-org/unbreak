-- |
-- Module       : Unbreak.Run
-- License      : AGPL-3
-- Maintainer   : Kinoru
-- Stability    : Provisional
-- Portability  : POSIX
--
-- Functions that perform the action of the Unbreak utility.
module Unbreak.Run
    ( runInit
    , runOpen
    , runLogout
    , runAdd
    ) where

import Prelude hiding ((++))
import Control.Monad
import Control.Exception
import System.IO
import System.IO.Error
import System.Exit

import System.Posix.ByteString

import Data.ByteString (ByteString)
import qualified Data.ByteString.OverheadFree as B
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.Text.Encoding as T

import Unbreak.Crypto
import Unbreak.Format

(++) :: Monoid m => m -> m -> m
(++) = mappend

data Session = Session
    { shelfPath :: !ByteString
    , master :: !ByteString
    }

sessionPath :: ByteString
sessionPath = "/tmp/unbreak.session"

getHomePath :: IO ByteString
getHomePath = getEnv "HOME" >>= \ m -> case m of
    Nothing -> error "$HOME not found"
    Just h -> return h

-- | Creates the @~\/.unbreak.json@ file with the default configuration
-- if it's missing.
runInit :: IO ()
runInit = do
    confPath <- (++ "/.unbreak.json") <$> getHomePath
    existence <- fileExist confPath
    if existence
    then B.putStrLn "The file ~/.unbreak.json already exists.\
        \ If you are willing to create the default config file,\
        \ please delete ~/.unbreak.json and retry.\n\
        \CAUTION: the \"name\" part of the config may be required to open\
        \ the documents you have created in the past."
    else do
        B.writeFile confPath (enc initConf)
        B.putStrLn $ "Created the initial default configuration at " ++
            confPath

-- idempotent session
session :: Conf -> IO Session
session Conf{..} = catchIOError
    -- get existing session
    ( do
        thisShelfPath <- B.readFile sessionPath
        thisMaster <- B.readFile (thisShelfPath ++ "/thisMaster")
        return $ Session thisShelfPath thisMaster
    )
    -- or if that fails, create a new session
    $ const $ do
        thisShelfPath <- mkdtemp "/dev/shm/unbreak-"
        B.writeFile sessionPath thisShelfPath
        B.putStr "Type password: "
        password <- withNoEcho B.getLine <* B.putStrLn ""
        let thisMaster = scrypt password (T.encodeUtf8 name)
        B.writeFile (thisShelfPath ++ "/thisMaster") thisMaster
        createDirectory (thisShelfPath ++ "/file") 0o700
        return $ Session thisShelfPath thisMaster

-- | Given a filename, try copying the file from the remote to a temporary
-- shared memory space, open it with the text editor specified in the config
-- file, and copy it back to the remote. Shell command @scp@ must exist.
runOpen :: ByteString -> IO ()
runOpen filename = getConf f (editRemoteFile filename)
  where
    f errmsg = B.putStrLn ("Failed: " ++ errmsg) *> exitFailure

getConf :: (ByteString -> IO a) -> (Conf -> Session -> IO a) -> IO a
getConf failure success = do
    confPath <- (++ "/.unbreak.json") <$> getHomePath
    existence <- fileExist confPath
    if existence
    then do
        rawConf <- B.readFile confPath
        case dec rawConf of
            Left errmsg -> failure $ B.pack errmsg
            Right conf -> session conf >>= success conf
    else do
        B.putStrLn "You may need to run 'unbreak init' first."
        failure "~/.unbreak.json does not exist"

editRemoteFile :: ByteString -> Conf -> Session -> IO ()
editRemoteFile fileName Conf{..} Session{..} = do
    -- copy the remote file to the shelf
    tryRun "scp" [remoteFilePath, encFilePath]
        -- if there is a file, decrypt it
        ( decrypt master <$> B.tail <$> B.readFile encFilePath >>=
            \ m -> case m of
            CryptoPassed plaintext -> B.writeFile filePath plaintext
            CryptoFailed e -> do
                B.putStrLn $ "Decryption failed. " ++ B.pack (show e)
                exitFailure
        )
        -- or open a new file
        $ \ _ -> do
            B.putStrLn "Create new file."
            B.writeFile filePath ""
    -- record the current time
    before <- epochTime
    -- edit the file in the shelf
    run (T.encodeUtf8 editor) [filePath] $ const $ do
        B.putStrLn "Editor exited abnormally. Editing cancelled."
        exitFailure
    -- check mtime to see if the file has been modified
    after <- modificationTime <$> getFileStatus filePath
    when (before < after) $ do
        -- encrypt the file
        encryptCopy master filePath encFilePath
        -- upload the file from the shelf to the remote
        run "scp" [encFilePath, remoteFilePath] $
            \ n -> do
                B.putStrLn $ mconcat
                    [ "[!] Upload failed. ("
                    , B.pack $ show n
                    , ")\nYour file is at:\n\n\t"
                    , filePath
                    , "\n\nIf you want to retry upload, try:\n\n\t"
                    , "unbreak add -f "
                    , filePath
                    , "\n"
                    ]
                removeLink encFilePath
                exitFailure
        removeLink encFilePath
    removeLink filePath
    B.putStrLn "Done."
  where
    encFileName = B64.encode $ encryptFileName master fileName
    filePath = mconcat [shelfPath, "/file/", fileName]
    encFilePath = mconcat [shelfPath, "/file/", encFileName]
    remoteFilePath = mconcat [T.encodeUtf8 remote, encFileName]

runLogout :: IO ()
runLogout = do
    shelfPath <- catchIOError (B.readFile sessionPath) $ \ e -> do
        if isDoesNotExistError e
        then B.putStrLn "You are not logged in."
        else B.putStrLn $ mconcat
            [ "Reading "
            , sessionPath
            , " has failed. ("
            , B.pack $ show e
            , ") Perhaps there is no active session?"
            ]
        exitFailure
    -- TODO: replace this with a more sensible system call
    run "rm" ["-rf", shelfPath] $ \ errorCode -> B.putStrLn $ mconcat
        [ "[!] Removing the session directory at "
        , shelfPath
        , " failed! ("
        , B.pack $ show errorCode
        , ") Please manually delete it."
        ]
    removeLink sessionPath

-- | Pick a local file, encrypt it, and send to the remote storage.
runAdd
    :: Bool -- ^ Force upload even when the file name already exists
    -> RawFilePath
    -> IO ()
runAdd force filePath = getConf f (encryptAndSend force filePath)
  where
    f errmsg = B.putStrLn ("Failed: " ++ errmsg) *> exitFailure

encryptAndSend :: Bool -> RawFilePath -> Conf -> Session -> IO ()
encryptAndSend force filePath Conf{..} Session{..} = if force then action else
    tryRun "ssh" [host, "test", "-e", docdir ++ encFileName]
        (B.putStrLn msg *> exitFailure) $ const action
  where
    msg = "The file name already exists in the storage. Cancelled."
    (host, cDocdir) = B.break (== ':') remoteB
    docdir = if B.head cDocdir == ':' then B.tail cDocdir else cDocdir
    remoteB = T.encodeUtf8 remote
    fileName = snd $ B.breakEnd (== '/') filePath
    encFileName = B64.encode $ encryptFileName master fileName
    encFilePath = mconcat [shelfPath, "/file/", encFileName]
    remoteFilePath = remoteB ++ encFileName
    action = do
        encryptCopy master filePath encFilePath
        -- upload the file from the shelf to the remote
        run "scp" [encFilePath, remoteFilePath] $
            \ n -> B.putStrLn $ mconcat
                ["Upload failed. (", B.pack $ show n, ")"]
        -- cleanup: remove the local temporary file
        removeLink encFilePath

encryptCopy :: ByteString -> RawFilePath -> RawFilePath -> IO ()
encryptCopy key sourcePath targetPath = do
    plaintext <- B.readFile sourcePath
    nonce <- getRandomBytes 12
    B.writeFile targetPath $
        -- adding the version number, for forward compatibility
        "\0" ++ throwCryptoError (encrypt nonce key plaintext)

withNoEcho :: IO a -> IO a
withNoEcho action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin False) (hSetEcho stdin old) action

run :: RawFilePath -> [ByteString] -> (Int -> IO ()) -> IO ()
run cmd args = tryRun cmd args (return ())

tryRun :: RawFilePath -> [ByteString] -> IO a -> (Int -> IO a) -> IO a
tryRun cmd args successHandler failHandler = do
    pid <- forkProcess $ executeFile cmd True args Nothing
    getProcessStatus True False pid >>= \ mstatus -> case mstatus of
        Just status -> case status of
            Exited exitCode -> case exitCode of
                ExitSuccess -> successHandler
                ExitFailure c -> failHandler c
            _ -> error $ show cmd
        Nothing -> error $ show cmd
