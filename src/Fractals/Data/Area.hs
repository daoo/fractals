-- |This module provides a definition of a rendering area. That is the relation
-- between screen space and fractal space.
module Fractals.Data.Area where

import Data.Complex
import Fractals.Data.Size
import Fractals.Data.Vec2

-- |An extent is a rectangular part of a 2d plane. It is defined such that adding 'delta' to
-- 'topLeft' 'size' times will give you the "bottom right" corner.
data Extent a = Extent
  { extentTopLeft :: !(Vec2 a)
  , extentSize    :: !(Vec2 Int)
  , extentDelta   :: !(Vec2 a)
  }

{-# INLINE getCenter #-}
getCenter :: (Num a, Fractional a) => Extent a -> Vec2 a
getCenter ext = extentTopLeft ext .+. (vfromIntegral (extentSize ext) ./ 2.0)

{-# INLINE transform #-}
transform :: (Num a, Fractional a) => Extent a -> Extent a -> Vec2 a -> Vec2 a
transform a b p = extentTopLeft b .+. ((p .-. extentTopLeft a) ./. extentDelta a) .*. extentDelta b

-- |Defines an rendering area.
-- The rendering area represents which part of the complex plane we are
-- rendering and the resolution of the rendered image.
data Area = Area
  { areaScreen  :: Size           -- ^ Size of the rendered image in pixels.
  , areaPlane   :: Complex Double -- ^ Size of the complex plane.
  , areaTopLeft :: Complex Double -- ^ Top left of the visible complex plane.
  , areaDelta   :: Complex Double -- ^ Complex distance between renderd pixels.
  } deriving Show

{-# INLINE getAreaCenter #-}
getAreaCenter :: Area -> Complex Double
getAreaCenter area = (px + pw / 2.0) :+ (py - ph / 2.0)
  where
    (pw:+ph) = areaPlane area
    (px:+py) = areaTopLeft area

-- |Construct an Area from render size, and a rectangle in the complex plane.
{-# INLINE fromRectangle #-}
fromRectangle :: Size -> Complex Double -> Complex Double -> Area
fromRectangle screen plane@(pw:+ph) topleft =
  Area screen plane topleft
    ((pw / realToFrac (width screen)) :+ (- ph / realToFrac (height screen)))

-- |Construct an Area from render size, the width of the complex plane and the
-- center point of the complex plane.
{-# INLINE fromAspectCentered #-}
fromAspectCentered :: Size -> Double -> Complex Double -> Area
fromAspectCentered screen pw (x:+y) = Area screen plane topleft delta
  where
    w             = realToFrac $ width screen
    h             = realToFrac $ height screen
    aspect        = w / h
    plane@(_:+ph) = pw :+ pw / aspect
    topleft       = (x - pw / 2) :+ (y + ph / 2)
    delta         = (pw / w) :+ (- ph / h)

{-# INLINE setPlaneCenter #-}
setPlaneCenter :: Complex Double -> Area -> Area
setPlaneCenter (x:+y) (Area screen plane@(pw:+ph) _ delta) =
  Area screen plane topleft delta
  where
    topleft = (x - (pw / 2)) :+ (y + (ph / 2))

{-# INLINE resizeScreen #-}
resizeScreen :: Size -> Area -> Area
resizeScreen ns area =
  fromAspectCentered ns (realPart $ areaPlane area) (getAreaCenter area)

{-# INLINE resizePlane #-}
resizePlane :: Complex Double -> Complex Double -> Area -> Area
resizePlane topleft plane@(pw:+ph) area = Area screen plane topleft delta
  where
    screen = areaScreen area
    w      = realToFrac $ width screen
    h      = realToFrac $ height screen
    delta  = (pw / w) :+ (- ph / h)

{-# INLINE screenToPlane #-}
screenToPlane :: Area -> Vec2 Int -> Complex Double
screenToPlane area (x :* y) = areaTopLeft area + (x'*dx :+ y'*dy)
  where
    x' = fromIntegral x
    y' = fromIntegral y
    dx :+ dy = areaDelta area
