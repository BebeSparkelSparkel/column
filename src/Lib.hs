module Lib where

import Control.Arrow ((>>>))
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty
import Data.Bifunctor (bimap)
import Data.Functor
import Control.Monad.State
import Lens.Micro.Mtl (zoom)
import Lens.Micro (_1, _2)


-- | Like splitCount but includes the maximum width of the initial splitters.
initSepSplitCount :: (Monad m, Eq a) => a -> [a] -> StateT (Int,[Int]) m [[a]]
initSepSplitCount splitter xs = do
  ys <- zoom _1 (initSep splitter xs)
  zs <- zoom _2 (splitCount splitter ys)
  pure zs

-- | Splits a single line up on the splitter element and increases the state column widths to the maximum width of that column
-- | Multiple adjacent split elements are grouped and splits just as a single splitter element would
-- | The splitter elements are excluded in the result.
splitCount :: forall m a. (Monad m, Eq a) => a -> [a] -> StateT [Int] m [[a]]
splitCount splitter cs = do
  i :| is <- gets $ fromMaybe (0 :| []) . nonEmpty
  put is
  (ys, i') <- sc cs
  modify (max i i' :)
  pure ys
  where
  sc :: [a] -> StateT [Int] m ([[a]],Int)
  sc = \case
    x:x':xs | x == splitter && x' /= splitter -> do
        i:|is <- gets $ fromMaybe (1 :| []) . nonEmpty
        put is
        (ys,i') <- sc (x':xs)
        modify (max i i' :)
        pure ([]:ys,1)
          | x == splitter && x' == splitter -> sc $ x':xs
    x:xs  | x /= splitter -> sc xs <&> bimap
            (\case (y:ys) -> (x:y):ys
                   [] -> [[x]] )
            (+ 1)
          | otherwise -> sc xs
    [] -> pure ([],1)

initSep :: forall m a. (Monad m, Eq a) => a -> [a] -> StateT Int m [a]
initSep splitter = (,0) >>> counter >>> \(xs,c) -> modify (max c) $> xs
  where
    counter :: ([a],Int) -> ([a],Int)
    counter = \case
      (x:xs,i) | x == splitter -> counter (xs,i+1)
      x -> x

initAlignColumns :: forall a. Int -> a -> [Int] -> [[a]] -> [a]
initAlignColumns initCount filler counts columns = leftPad filler initCount $ alignColumns filler  counts columns

alignColumns :: forall a. a -> [Int] -> [[a]] -> [a]
alignColumns filler = ac
  where
  ac :: [Int] -> [[a]] -> [a]
  ac _      [xs]   = xs
  ac (i:is) (x:xs) = rightPad filler i x $ ac is xs
  ac _      _      = []

rightPad :: forall a. a -> Int -> [a] -> [a] -> [a]
rightPad padding count toPad end = evalState (rp toPad) count
  where
  rp :: [a] -> State Int [a]
  rp = \case
    x:xs -> (x :) <$> (modify (subtract 1) *> rp xs)
    [] -> get <&> \c -> leftPad padding c end

leftPad :: a -> Int -> [a] -> [a]
leftPad padding count end
  | count > 0 = padding : leftPad padding (count-1) end
  | otherwise = end

