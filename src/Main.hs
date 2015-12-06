module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as B
import System.Console.CmdArgs.Explicit

import Unbreak.Run

data Cmd
    = CmdOpen ByteString
    | CmdHelp

arguments :: Mode Cmd
arguments = mode "unbreak" (CmdOpen "")
    "insert appropriate intro here" -- FIXME
    (flagArg argUpd "FILENAME")
    [ {- flagNone ["init", "i"]
        (const CmdInit) "Create the default configuration file at\
            \ ~/.e.xtendo.org/unbreak.json"
    , -} flagHelpSimple (const CmdHelp)
    ]
  where
    argUpd a _ = Right $ CmdOpen (pack a)
    pack = LB.toStrict . B.toLazyByteString . B.stringUtf8

main :: IO ()
main = do
    args <- processArgs arguments
    case args of
        CmdHelp     -> print $ helpText [] HelpFormatDefault arguments
        CmdOpen b   -> if B.length b == 0 then runInit else runOpen b
