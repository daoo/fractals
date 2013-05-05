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
fromRectangle :: (R, R) -> (R, R) -> (Int, Int) -> Area
fromRectangle topleft plane@(pw, ph) screen@(w, h) =
  Area topleft plane (dx, dy) screen
  where
    dx =   pw / realToFrac w
    dy = - ph / realToFrac h
