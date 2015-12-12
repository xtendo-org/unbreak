module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as B
import System.Console.CmdArgs.Explicit

import Unbreak.Run

pack :: String -> ByteString
pack = LB.toStrict . B.toLazyByteString . B.stringUtf8

data Cmd
    = CmdInit
    | CmdOpen ByteString
    | CmdLogout
    | CmdHelp

modeInit :: Mode Cmd
modeInit = mode "init" CmdInit "create the default config file"
    (flagArg (\_ c -> Right c) "") []

modeOpen :: Mode Cmd
modeOpen = mode "open" (CmdOpen "") "open a remote encrypted file"
    (flagArg argUpd "FILENAME") []
  where
    argUpd a _ = Right $ CmdOpen (pack a)

modeLogout :: Mode Cmd
modeLogout = mode "logout" CmdLogout "delete the current session"
    (flagArg (\_ c -> Right c) "") []

arguments :: Mode Cmd
arguments = modes "unbreak" CmdHelp
    "remote, accessible, and encrypted file storage utility"
    [modeInit, modeOpen, modeLogout]

main :: IO ()
main = do
    args <- processArgs arguments
    case args of
        CmdHelp     -> print $ helpText [] HelpFormatDefault arguments
        CmdInit     -> runInit
        CmdOpen b   -> if B.length b == 0
            then error "file name can't be empty"
            else runOpen b
        CmdLogout   -> runLogout
