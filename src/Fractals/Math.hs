module Fractals.Math
  (
  -- * Vector
    Vec(Vec)
  , Point

  -- * Size
  , Size(width, height)
  , mkSize
  , sizeArea

  -- * Rectangle
  , Rectangle(..)
  , fixAspect

  -- * Algebra
  , square
  , scale
  , lerp
  , lerpf
  , lerps
  ) where

import Data.Word
import GHC.Base

-- |Definition of a 2d 'Int' vector.
data Vec = Vec !Int !Int
  deriving Show

-- |A 'Point' is isomorphic to a 'Vec'.
type Point = Vec

-- |Defintion of addition for 'Vec'.
--
-- Provieded as a new operator because implementing 'Num' is non-trivial.
(.+.) :: Vec -> Vec -> Vec
(.+.) (Vec x1 y1) (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)

-- |Definition of the size of a rectangle in 2D space.
data Size = Size { width :: !Int, height :: !Int }
  deriving Show

-- |Construct a 'Size'.
--
-- Using any 'Word' that don't fit in a 'Int' is undefined.
mkSize :: Word -> Word -> Size
mkSize w h = Size (fromIntegral w) (fromIntegral h)

-- |Calculate the area of a 'Size'.
sizeArea :: Size -> Int
sizeArea (Size w h) = w * h

-- |Represents a rectangle using two corner points.
--
-- In any 2 dimensional interpretation of a rectangle the first point ('rectA')
-- is always top left and the second point ('rectB') is always bottom right.
data Rectangle = Rectangle
  { rectA :: !Point -- ^Top left point
  , rectB :: !Point -- ^Bottom right point
  } deriving Show

-- |Construct a 'Rectangle' from two points.
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
fixAspect :: Size   -- ^Aspect ratio given by size
           -> Point -- ^Point a
           -> Point -- ^Point b
           -> Rectangle -- ^The largest rectangle containing both point a and b.
fixAspect aspect a b = fromPoints a (a .+. findLargest aspect a b)

-- |Definition of linear interpolation.
lerp :: Integral a => Int    -- ^Number of steps
                   -> (a, a) -- ^Interpolation range
                   -> Int    -- ^Offset
                   -> a      -- ^Interpolated value
lerp steps (a, b) x = a + (fromIntegral x * (b-a)) `div` fromIntegral steps

lerpf :: (Num a, Fractional a) => Int -> (a, a) -> Int -> a
lerpf steps (a, b) x = a + (fromIntegral x * (b-a)) / fromIntegral steps

lerps :: (Fractional a, Num a) => Int -> (a, a) -> [a]
lerps steps range = map f [0..steps-1]
  where
    f = lerpf (steps-1) range . fromIntegral

-- |Square a number.
square :: Num a => a -> a
square x = x * x

-- |Scale a number from one range to another.
-- Both ranges are inclusive, start at 0 and ends at the given number. This
-- function is tuned for speed and does not do any division by zero or range
-- checks.
--
-- prop> forall a b i. scale a b 0 == 0
-- prop> forall a b i. scale a b a == b
scale :: Int -- ^ End of the first range @[0, a]@, must be greater than zero
      -> Int -- ^ End of the second range @[0, b]@, must be greater than zero
      -> Int -- ^ The number within range @[0, a]@
      -> Int -- ^ Number in range @[0, b]@
scale a b i = (i * b) `quotInt` a
