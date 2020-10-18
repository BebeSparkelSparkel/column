module Lib where

import Data.Maybe (fromMaybe)
import Data.List.NonEmpty
import Data.Bifunctor (bimap)
import Data.Functor
import Control.Monad.State


splitCount :: forall m a. (Monad m, Eq a) => a -> [a] -> StateT [Int] m [[a]]
splitCount s cs = do
  i:|is <- gets $ fromMaybe (1 :| []) . nonEmpty
  put is
  (ys, i') <- sc cs
  modify (max i i' :)
  pure ys
  where
  sc :: [a] -> StateT [Int] m ([[a]],Int)
  sc (x:x':xs) | x == s = if x' /= s
    then do
      i:|is <- gets $ fromMaybe (1 :| []) . nonEmpty
      put is
      (ys,i') <- sc (x':xs)
      modify (max i i' :)
      pure ([]:ys,1)
    else sc $ x':xs
  sc (x:xs)
    | x /= s = sc xs <&> bimap
      (\case (y:ys) -> (x:y):ys
             [] -> [[x]] )
      (+ 1)
    | otherwise = sc xs
  sc [] = pure ([],1)

rightPad :: forall a. a -> [Int] -> [[a]] -> [a]
rightPad filler = rp
  where
  rp :: [Int] -> [[a]] -> [a]
  rp _      [xs]        = xs
  rp (i:is) ((x:xs):ys) = x : rp (i-1:is) (xs:ys)
  rp (i:is) ([]:ys)     = leftPad i filler $ rp is ys
  rp []     ((x:y):ys)  = x : rp [] (y:ys)
  rp []     ([]:ys)     = rp [] ys
  rp _      []          = []
  leftPad :: Int -> a -> [a] -> [a]
  leftPad i x t | i > 0 = x : leftPad (i - 1) x t
                | otherwise = t

