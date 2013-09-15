module Fractals.Area
  ( Area(..)
  , getAreaCenter
  , fromRectangle
  , fromAspectCentered
  , resizeScreen
  , resizePlane
  , screenToPlane
  ) where

import Fractals.Complex

-- |Defines an rendering area.
-- The rendering area represents which part of the complex plane we are
-- rendering and the resolution of the rendered image.
data Area = Area
  { areaScreen  :: (Int, Int) -- ^ Size of the rendered image in pixels.
  , areaPlane   :: Comp       -- ^ Size of the complex plane.
  , areaTopLeft :: Comp       -- ^ Top left of the visible complex plane.
  , areaDelta   :: Comp       -- ^ Complex distance between renderd pixels.
  } deriving Show

{-# INLINE getAreaCenter #-}
getAreaCenter :: Area -> Comp
getAreaCenter area = px + pw / 2.0 :+ py - ph / 2.0
  where
    (pw:+ph) = areaPlane area
    (px:+py) = areaTopLeft area

-- |Construct an Area from render size, and a rectangle in the complex plane.
{-# INLINE fromRectangle #-}
fromRectangle :: (Int, Int) -> Comp -> Comp -> Area
fromRectangle screen@(w, h) plane@(pw:+ph) topleft =
  Area screen plane topleft (pw / realToFrac w :+ - ph / realToFrac h)

-- |Construct an Area from render size, the width of the complex plane and the
-- center point of the complex plane.
{-# INLINE fromAspectCentered #-}
fromAspectCentered :: (Int, Int) -> R -> Comp -> Area
fromAspectCentered screen@(w, h) pw (x:+y) = Area screen plane topleft delta
  where
    (w', h')      = (realToFrac w, realToFrac h)
    aspect        = w' / h'
    plane@(_:+ph) = pw :+ pw / aspect
    topleft       = x - pw / 2 :+ y + ph / 2
    delta         = pw / w' :+ - ph / h'

{-# INLINE resizeScreen #-}
resizeScreen :: (Int, Int) -> Area -> Area
resizeScreen ns area =
  fromAspectCentered ns (realPart $ areaPlane area) (getAreaCenter area)

{-# INLINE resizePlane #-}
resizePlane :: Comp -> Comp -> Area -> Area
resizePlane topleft plane@(pw:+ph) area = Area screen plane topleft delta
  where
    screen@(w, h) = areaScreen area
    delta         = (pw / realToFrac w) :+ (- ph / realToFrac h)

{-# INLINE screenToPlane #-}
screenToPlane :: Area -> (Int, Int) -> Comp
screenToPlane area (x, y) = areaTopLeft area + (x'*dx :+ y'*dy)
  where
    x' = fromIntegral x
    y' = fromIntegral y
    dx :+ dy = areaDelta area
