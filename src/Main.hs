module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import System.Console.CmdArgs.Explicit

import Run

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
    argUpd a _ = Right $ CmdOpen (B.pack a)

main :: IO ()
main = do
    args <- processArgs arguments
    case args of
        CmdHelp     -> print $ helpText [] HelpFormatDefault arguments
        CmdOpen b   -> if B.length b == 0 then runInit else
            print $ mconcat ["I should open ", b] -- FIXME
