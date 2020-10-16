module Lib where

import Lens.Micro.Mtl ((%=))
import Lens.Micro.TH (makeLenses)


data Split = Split
  { _consumingSplit :: Bool
  , _counts :: [Int]
  }
makeLenses ''Split

defaultSplit :: Split
defaultSplit = Split
  { _consumingSplit = False
  , _counts :: [Int]
  }

split :: Eq a => a -> [a] -> ([[a]], [Int])
split s = flip runState defaultSplit 
  where
  sp :: [a] -> ([Int],[[a]])
  sp (x:xs) | x /= s    = sp xs <&> inc x
            | otherwise = sp xs

  inc x (i:is,xs) = (i + 1 : is, x : xs)
  inc _ x = x
