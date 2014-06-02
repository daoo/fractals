{-# LANGUAGE CPP #-}
module Fractals.Complex
  ( Data.Complex.Complex(..)
  , Data.Complex.realPart
  , Data.Complex.imagPart
  , R
  , magnitudeSquared
  , (./)
  ) where

import Data.Complex

#ifdef RATIONAL
import Data.Ratio
type R = Rational
#else
type R = Double
#endif

{-# INLINE magnitudeSquared #-}
-- |The magintude of a complex number squared.
-- Avoids the expensive square root.
magnitudeSquared :: Complex R -> R
magnitudeSquared (a :+ b) = a*a + b*b

{-# INLINE (./) #-}
(./) :: Complex R -> R -> Complex R
(a:+b) ./ x = (a/x) :+ (b/x)
