module Fractals.Area
  ( Area(..)
  , fromRectangle
  ) where

import Fractals.Complex

-- |Defines an rendering area.
-- The rendering area represents which part of the complex plane we are
-- rendering and the resolution of the rendered image.
data Area = Area
  { areaScreen  :: (Int, Int)
  , areaPlane   :: Comp
  , areaTopLeft :: Comp
  , areaDelta   :: Comp
  }

-- |Construct a Area from render size, and a rectangle in the complex plane.
{-# INLINE fromRectangle #-}
fromRectangle :: (Int, Int) -> Comp -> Comp -> Area
fromRectangle screen@(w, h) plane@(pw:+ph) topleft =
  Area screen plane topleft (pw / realToFrac w :+ - ph / realToFrac h)
