module Main where

import Options.Commander (command_, toplevel, optDef, raw)
import System.IO (isEOF, hGetLine, stdin, stdout)
import Control.Monad.Loops (untilM)
import Control.Monad.State (runStateT, get, lift)
import Lib (rightPad, splitCount)
import Data.Foldable (for_)


main :: IO ()
main = command_
  . toplevel @"argument-taker"
  . optDef @"s" @"seperator" ' ' $ \seperator ->
    optDef @"f" @"fill character" seperator $ \filler -> raw $ do
      (cs,is) <- flip runStateT [] $ flip untilM (lift isEOF) $ do
        l <- lift $ hGetLine stdin
        splitCount seperator l
      for_ cs $ putStrLn . rightPad filler is

