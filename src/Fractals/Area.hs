module Fractals.Area
  ( Area(..)
  , fromRectangle
  ) where

import Fractals.Complex

data Area = Area
  { areaTopLeft :: (R, R)
  , areaPlane   :: (R, R)
  , areaDelta   :: (R, R)
  , areaScreen  :: (Int, Int)
  }

{-# INLINE fromRectangle #-}
fromRectangle :: (Int, Int) -> (R, R) -> (R, R) -> Area
fromRectangle screen@(w, h) topleft plane@(pw, ph) =
  Area topleft plane (dx, dy) screen
  where
    dx =   pw / realToFrac w
    dy = - ph / realToFrac h
