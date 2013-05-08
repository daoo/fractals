module Fractals.Area
  ( Area(..)
  , fromRectangle
  ) where

import Fractals.Complex

data Area = Area
  { areaScreen  :: (Int, Int)
  , areaPlane   :: (R, R)
  , areaTopLeft :: (R, R)
  , areaDelta   :: (R, R)
  }

{-# INLINE fromRectangle #-}
fromRectangle :: (Int, Int) -> (R, R) -> (R, R) -> Area
fromRectangle screen@(w, h) plane@(pw, ph) topleft =
  Area screen plane topleft (pw / realToFrac w, - ph / realToFrac h)
