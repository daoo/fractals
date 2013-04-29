module Tests where

import Extensions
import Test.QuickCheck

propIterateN :: Int -> Property
propIterateN a = forAll (choose (0, 10000)) $
  \i -> take i (iterate (+1) a) == iterateN i (+1) a
