{-# LANGUAGE MagicHash #-}
module Fractals.Utility where

import GHC.Exts

-- |Square a number
{-# INLINE square #-}
square :: Num a => a -> a
square x = x * x

{-# INLINE scale #-}
-- |Scale a number from one range to another.
-- Both ranges are inclusive, start at 0 and ends at the given number. This
-- function is tuned for speed and does not do any division by zero or range
-- checks.
scale :: Int -- ^ End of the first range [0, a], must be greater than zero
      -> Int -- ^ End of the second range [0, b], must be greater than zero
      -> Int -- ^ The number within range [0, a]
      -> Int
scale a b i = (i * a) `unsafeQuot` (b + 1)
  where
    unsafeQuot (I# x) (I# y) = I# (quotInt# x y)
