module Fractals.Data.Rectangle
  ( Rectangle(Rectangle)
  , fixAspect
  ) where

import Fractals.Data.Size
import Fractals.Data.Vec2
import GHC.Base

-- |Represents a rectangle using two corner points.
--
-- In any 2 dimensional interpretation of a rectangle the first point ('rectA')
-- is always top left and the second point ('rectB') is always bottom right.
data Rectangle = Rectangle
  !Vec -- ^Top left point
  !Vec -- ^Bottom right point
  deriving Show

{-# INLINE fromPoints #-}
-- |Construct a 'Rectangle' from two points.
fromPoints :: Vec -> Vec -> Rectangle
fromPoints (Vec x1 y1) (Vec x2 y2) =
  Rectangle (Vec xmin ymin) (Vec xmax ymax)
  where
    (xmin, xmax) = minmax x1 x2
    (ymin, ymax) = minmax y1 y2

    minmax a b | a <= b    = (a, b)
               | otherwise = (b, a)

{-# INLINE findLargest #-}
-- |Find the largest (by area) size that goes through two points and has the
-- specific aspect ratio.
findLargest :: Size -> Vec -> Vec -> Vec
findLargest (Size w h) (Vec x1 y1) (Vec x2 y2)
  | rw*rh' < rw'*rh = byh
  | otherwise       = byw
  where
    rw = x2 - x1
    rh = y2 - y1

    rh' = (rw*h) `quotInt` assert (w>0) w
    rw' = (rh*w) `quotInt` assert (h>0) h

    byw = Vec rw  rh'
    byh = Vec rw' rh

{-# INLINE fixAspect #-}
-- |Resize a rectangle to the largest rectangle that goes through the bottom
-- left point with the specified aspect ratio.
fixAspect :: Size      -- ^Aspect ratio given by size
          -> Vec       -- ^Point a
          -> Vec       -- ^Point b
          -> Rectangle -- ^The largest rectangle containing both point a and b.
fixAspect aspect a b = fromPoints a (a .+. findLargest aspect a b)
