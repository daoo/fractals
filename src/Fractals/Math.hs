module Fractals.Math
  ( Vec(Vec)
  , Size(width, height)
  , mkSize
  , Point
  , Rectangle(..)
  , sizeArea
  , fixAspect
  , square
  , scale
  , lerp
  ) where

import GHC.Base

data Vec = Vec !Int !Int
  deriving Show

data Size = Size { width :: !Int, height :: !Int }
  deriving Show

mkSize :: Int -> Int -> Size
mkSize w h = assert (w > 0 && h > 0) (Size w h)

type Point = Vec

(.+) :: Vec -> Vec -> Vec
(.+) (Vec x1 y1) (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)

-- |Represents a rectangle from two corner points.
--
-- The first point is always top left and the second point is always bottom
-- right.
data Rectangle = Rectangle
  { rectA :: !Point
  , rectB :: !Point
  } deriving Show

-- |Calculate the area of the size.
sizeArea :: Size -> Int
sizeArea (Size w h) = w * h

fromPoints :: Point -> Point -> Rectangle
fromPoints (Vec x1 y1) (Vec x2 y2) =
  Rectangle (Vec xmin ymin) (Vec xmax ymax)
  where
    (xmin, xmax) = minmax x1 x2
    (ymin, ymax) = minmax y1 y2

    minmax a b | a <= b    = (a, b)
               | otherwise = (b, a)

-- |Find the largest (by area) size that goes through two points and has the
-- specific aspect ratio.
findLargest :: Size -> Point -> Point -> Vec
findLargest (Size w h) (Vec x1 y1) (Vec x2 y2)
  | rw*rh' < rw'*rh = byh
  | otherwise       = byw
  where
    rw = x2 - x1
    rh = y2 - y1

    rh' = rw*h `quot` w
    rw' = rh*w `quot` h

    byw = Vec rw  rh'
    byh = Vec rw' rh

-- |Resize a rectangle to the largest rectangle that goes through the bottom
-- left point with the specified aspect ratio.
fixAspect :: Size -> Point -> Point -> Rectangle
fixAspect aspect a b = fromPoints a (a .+ findLargest aspect a b)

lerp :: Integral a => a -> (a, a) -> a -> a
lerp steps (a, b) x = (a*(steps-x) + b*x) `div` steps

-- |Square a number.
square :: Num a => a -> a
square x = x * x

-- |Scale a number from one range to another.
-- Both ranges are inclusive, start at 0 and ends at the given number. This
-- function is tuned for speed and does not do any division by zero or range
-- checks.
scale :: Int -- ^ End of the first range [0, a], must be greater than zero
      -> Int -- ^ End of the second range [0, b], must be greater than zero
      -> Int -- ^ The number within range [0, a]
      -> Int -- ^ Number in range [0, b]
scale a b i = (i * b) `quotInt` a
