module Main where

import Options.Commander
import System.IO (isEOF, hGetLine, stdin, stdout)
import Control.Monad.Loops (untilM_)


main :: IO ()
main = command_
  . toplevel @"argument-taker"
  . optDef @"s" @"seperator" ' ' \seperator ->
    optDef @"f" @"fill character" seperator \filler -> raw $ untilM_
      do
        l <- hGetLine stdin
        
      isEOF

