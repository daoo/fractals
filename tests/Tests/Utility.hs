module Tests.Utility where

import Control.Monad
import Fractals.Utility
import Test.SmallCheck
import Test.SmallCheck.Series

inRange a = series >>= \x -> guard (x >= 0 && x <= a) >> return x

propScaleInRange (Positive a) (Positive b) = over (inRange a) $ \i ->
  let i' = scale a b i in i' >= 0 && i' <= b

propScale0 (Positive a) (Positive b) = exists $ over (inRange a) $ \i ->
  scale a b i == 0

propScaleB (Positive a) (Positive b) = exists $ over (inRange a) $ \i ->
  scale a b i == b
