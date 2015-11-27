module Run
    ( runInit
    ) where

import Prelude hiding ((++))

import System.Posix.ByteString

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Format

(++) :: Monoid m => m -> m -> m
(++) = mappend

getHomePath :: IO ByteString
getHomePath = getEnv "HOME" >>= \ m -> case m of
    Nothing -> error "$HOME not found"
    Just h -> return h

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
