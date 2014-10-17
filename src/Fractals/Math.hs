{-# LANGUAGE Unsafe, BangPatterns, MagicHash #-}
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
  , unsafeScaleInt
  , lerp
  , unsafeLerpWord
  , lerpFractional
  , lerpFractionals
  ) where

import Data.Word
import GHC.Base

-- $setup
-- >>> import Test.QuickCheck
-- >>> import System.Random
-- >>>
-- >>> let genInRange (a, b) = choose (a, b)
-- >>> let genLarger a = sized $ \n -> choose (a+1, a + max 2 (abs $ a * fromIntegral n))
-- >>> let genRange = arbitrarySizedIntegral >>= \a -> genLarger a >>= \b -> return (a, b)
-- >>> let genSteps (a, b) = genInRange (1, abs (b-a))
-- >>>
-- >>> let forAllLerp f = forAll genRange $ \r -> forAll (genSteps r) $ \s -> forAll (genInRange (0, s)) $ \x -> f s r x
-- >>> let forAllIn r f = forAll (choose r) f

-- |
-- prop> \a -> forAll (genLarger a) $ \b -> (b :: Int) > (a :: Int)
-- prop> forAll genRange $ \(a, b) -> (a :: Int) < (b :: Int)

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
fixAspect :: Size      -- ^Aspect ratio given by size
          -> Point     -- ^Point a
          -> Point     -- ^Point b
          -> Rectangle -- ^The largest rectangle containing both point a and b.
fixAspect aspect a b = fromPoints a (a .+. findLargest aspect a b)

{-# SPECIALIZE INLINE lerp :: Int -> (Int, Int) -> Int -> Int #-}
-- |Definition of linear interpolation.
--
-- prop> forAllLerp $ \s (a, b) _ -> lerp s (a, b) 0 == a
-- prop> forAllLerp $ \s (a, b) _ -> lerp s (a, b) s == b
-- prop> forAllLerp $ \s (a, b) x -> lerp s (a, b) x >= a
-- prop> forAllLerp $ \s (a, b) x -> lerp s (a, b) x <= b
lerp :: Integral a => Int    -- ^Number of steps
                   -> (a, a) -- ^Interpolation range
                   -> Int    -- ^Offset
                   -> a      -- ^Interpolated value
lerp steps (a, b) x = a + ((fromIntegral x * (b-a)) `div` fromIntegral steps)

-- |Unsafe linear interpolation, does not check for division by zero.
--
-- prop> forAllLerp $ \s (a, b) x -> unsafeLerpWord s (a, b) 0 == a
-- prop> forAllLerp $ \s (a, b) x -> unsafeLerpWord s (a, b) s == b
-- prop> forAllLerp $ \s (a, b) x -> unsafeLerpWord s (a, b) x >= a
-- prop> forAllLerp $ \s (a, b) x -> unsafeLerpWord s (a, b) x <= b
-- prop> forAllLerp $ \s (a, b) x -> fromIntegral (unsafeLerpWord s (a, b) x) == lerp (fromIntegral s) ((fromIntegral a) :: Int, fromIntegral b) (fromIntegral x)
unsafeLerpWord :: Word -> (Word, Word) -> Word -> Word
unsafeLerpWord (W# s) ((W# a), (W# b)) (W# x) = W# (plusWord# a (quotWord# (timesWord# x (minusWord# b a)) s))

{-# SPECIALIZE INLINE lerpFractional :: Int -> (Float , Float ) -> Int -> Float  #-}
{-# SPECIALIZE INLINE lerpFractional :: Int -> (Double, Double) -> Int -> Double #-}
lerpFractional :: (Num a, Fractional a) => Int -> (a, a) -> Int -> a
lerpFractional steps (a, b) x = a + (fromIntegral x * (b-a)) / fromIntegral steps

lerpFractionals :: (Fractional a, Num a) => Int -> (a, a) -> [a]
lerpFractionals steps range = map f [0..steps-1]
  where
    f = lerpFractional (steps-1) range . fromIntegral

-- |Square a number.
--
-- prop> square 0 == 0
-- prop> square 1 == 1
-- prop> \x -> square (x :: Int) >= 0
-- prop> \x -> square (x :: Int) == square (-x)
square :: Num a => a -> a
square x = x * x

-- |Scale an int from one integer range to another.
--
-- Both ranges are inclusive, start at 0 and ends at the given number. This
-- function is tuned for speed and does not do any division by zero or range
-- checks.
--
-- prop> \(Positive a) (Positive b) -> unsafeScaleInt a b 0 == 0
-- prop> \(Positive a) (Positive b) -> unsafeScaleInt a b a == b
-- prop> \(Positive a) (Positive b) -> forAllIn (0, a) $ \i -> unsafeScaleInt a b i >= 0
-- prop> \(Positive a) (Positive b) -> forAllIn (0, a) $ \i -> unsafeScaleInt a b i <= b
unsafeScaleInt :: Int -- ^ @a@, end of the first range @[0, a]@, must be greater than zero
               -> Int -- ^ @b@, end of the second range @[0, b]@, must be greater than zero
               -> Int -- ^ The number within range @[0, a]@
               -> Int -- ^ Number in range @[0, b]@
unsafeScaleInt a b i = (i * b) `quotInt` a
