module Fractals.Math where

import GHC.Base

{-# INLINE unsafeScaleInt #-}
-- |Scale an int from one integer range to another.
--
-- Both ranges are inclusive, start at 0 and ends at the given number. This
-- function is tuned for speed and does not do any division by zero or range
-- checks.
--
-- prop> \(Positive a) (Positive b) -> unsafeScaleInt a b 0 == 0
-- prop> \(Positive a) (Positive b) -> unsafeScaleInt a b a == b
-- prop> \(Positive a) (Positive b) -> forAll (choose (0, a)) $ \i -> unsafeScaleInt a b i >= 0
-- prop> \(Positive a) (Positive b) -> forAll (choose (0, a)) $ \i -> unsafeScaleInt a b i <= b
unsafeScaleInt :: Int -- ^ @a@, end of the first range @[0, a]@, must be greater than zero
               -> Int -- ^ @b@, end of the second range @[0, b]@, must be greater than zero
               -> Int -- ^ The number within range @[0, a]@
               -> Int -- ^ Number in range @[0, b]@
unsafeScaleInt a b i = (i * b) `quotInt` a
