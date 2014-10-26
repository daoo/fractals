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
  !(Vec2 Int) -- ^Top left point
  !(Vec2 Int) -- ^Bottom right point
  deriving Show

{-# INLINE fromPoints #-}
-- |Construct a 'Rectangle' from two points.
fromPoints :: Vec2 Int -> Vec2 Int -> Rectangle
fromPoints (x1 :* y1) (x2 :* y2) =
  Rectangle (xmin :* ymin) (xmax :* ymax)
  where
    (xmin, xmax) = minmax x1 x2
    (ymin, ymax) = minmax y1 y2

    minmax a b | a <= b    = (a, b)
               | otherwise = (b, a)

{-# INLINE findLargest #-}
-- |Find the largest (by area) size that goes through two points and has the
-- specific aspect ratio.
findLargest :: Size -> Vec2 Int -> Vec2 Int -> Vec2 Int
findLargest (Size w h) (x1 :* y1) (x2 :* y2)
  | rw*rh' < rw'*rh = byh
  | otherwise       = byw
  where
    rw = x2 - x1
    rh = y2 - y1

    rh' = (rw*h) `quotInt` assert (w>0) w
    rw' = (rh*w) `quotInt` assert (h>0) h

    byw = rw  :* rh'
    byh = rw' :* rh

{-# INLINE fixAspect #-}
-- |Resize a rectangle to the largest rectangle that goes through the bottom
-- left point with the specified aspect ratio.
fixAspect :: Size      -- ^Aspect ratio given by size
          -> Vec2 Int  -- ^Point a
          -> Vec2 Int  -- ^Point b
          -> Rectangle -- ^The largest rectangle containing both point a and b.
fixAspect aspect a b = fromPoints a (a .+. findLargest aspect a b)
