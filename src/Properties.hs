module Main where

import Control.Monad
import Fractals.Utility
import Test.SmallCheck
import Test.SmallCheck.Series

inRange a = series >>= \x -> guard (x >= 0 && x <= a) >> return x

propInRange (Positive a) (Positive b) = over (inRange a) $ \i ->
  i >= a && i <= b

propScaleInRange (Positive a) (Positive b) = over (inRange a) $ \i ->
  let i' = scale a b i in i' >= 0 && i' <= b

propScale0 (Positive a) (Positive b) = exists $ over (inRange a) $ \i ->
  scale a b i == 0

propScaleB (Positive a) (Positive b) = exists $ over (inRange a) $ \i ->
  scale a b i == b

main = do
  smallCheck 5 propScaleInRange
  smallCheck 5 propScale0
  smallCheck 5 propScaleB
