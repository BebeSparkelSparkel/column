module Main where

import Options.Commander (command_, toplevel, optDef, raw)
import System.IO (isEOF, hGetLine, stdin, stdout)
import Control.Monad.Loops (untilM)
import Control.Monad.State (runStateT, get, lift)
import Lib
import Data.Foldable (for_)


main :: IO ()
main = command_
  . toplevel @"argument-taker"
  . optDef @"s" @"seperator" ' ' $ \seperator ->
    optDef @"f" @"fill character" seperator $ \filler -> raw $ do
      (rows,(initSeps,widths)) <- flip runStateT (0,[]) $ flip untilM (lift isEOF)
        $   lift (hGetLine stdin)
        >>= initSepSplitCount seperator
      for_ rows $ putStrLn . initAlignColumns initSeps filler widths

