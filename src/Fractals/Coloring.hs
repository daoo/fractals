{-# LANGUAGE BangPatterns #-}
module Fractals.Coloring
  (
    -- * ASCII
    ascii
    -- * Greyscale
  , greyscale
    -- * Palettes
  , mkColorMap
  , unsafeColor
  , colors1
  , colors2
  , colors3
  , colors4
  , colors5
  ) where

import Codec.Picture.Types
import Data.Vector.Storable as V (Vector, fromList, length)
import Fractals.Math
import GHC.Base

{-# INLINE ascii #-}
-- |Find a ASCII character representation of a number of iterations.
ascii :: Int -- ^Maximum number of iterations
      -> Int -- ^Number of iterations for some pixel
      -> Char
ascii m i = case unsafeScaleInt m 10 i of
  0  -> ' '
  1  -> '-'
  2  -> '~'
  3  -> '+'
  4  -> '*'
  5  -> '='
  6  -> '#'
  7  -> '%'
  8  -> '@'
  9  -> '&'
  10 -> '$'
  _  -> 'E'

{-# INLINE greyscale #-}
-- |Find a single channel 8-bit color representation of a number of iterations.
greyscale :: Int -- ^Maximum number of iterations
          -> Int -- ^Number of iterations for som pixel
          -> Pixel8
greyscale m i = fromIntegral $ unsafeScaleInt m 255 i

{-# INLINE unsafeColor #-}
unsafeColor :: Vector (PixelBaseComponent PixelRGB8) -> Int -> PixelRGB8
unsafeColor p i = unsafePixelAt p (i*3)

mkColorMap :: Int -> [PixelRGB8] -> Vector (PixelBaseComponent PixelRGB8)
mkColorMap m p = fromList $ concatMap (conv . index) [0..m]
  where
    p' = fromList $ concatMap conv p

    conv (PixelRGB8 r g b) = [r, g, b]

    index i | i == m    = unsafePixelAt p' 0
            | otherwise = unsafePixelAt p' ((i*3) `remInt` V.length p')

{-# INLINE interpolate #-}
interpolate :: PixelRGB8 -> PixelRGB8 -> [PixelRGB8]
interpolate (PixelRGB8 !r1 !g1 !b1) (PixelRGB8 !r2 !g2 !b2) =
  [ f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7, f 8, f 9, f 10, f 11 ]

  where
    s = 11

    f i     = PixelRGB8 (g r1 r2 i) (g g1 g2 i) (g b1 b2 i)
    g c1 c2 = fromIntegral . unsafeLerpWord s (fromIntegral c1, fromIntegral c2)

{-# INLINE (#!) #-}
(#!) :: PixelRGB8 -> (PixelRGB8 -> (PixelRGB8, [PixelRGB8])) -> [PixelRGB8]
c1 #! f = interpolate c1 c2 ++ cs
  where
    (c2, cs) = f c1

{-# INLINE (#:) #-}
(#:) :: PixelRGB8 -> (PixelRGB8 -> (PixelRGB8, [PixelRGB8])) -> PixelRGB8 -> (PixelRGB8, [PixelRGB8])
ci #: f = \c1 -> let (cj, cs) = f c1 in (ci, interpolate ci cj ++ cs)

{-# INLINE (#~) #-}
(#~) :: PixelRGB8 -> PixelRGB8 -> PixelRGB8 -> (PixelRGB8, [PixelRGB8])
cn1 #~ cn2 = \c1 -> (cn1, interpolate cn1 cn2 ++ interpolate cn2 c1)

infixr 2 #!
infixr 3 #:
infixr 4 #~

colors1 :: [PixelRGB8]
colors1 =
  PixelRGB8 0   10  20  #!
  PixelRGB8 50  100 240 #:
  PixelRGB8 20  3   26  #:
  PixelRGB8 230 60  20  #:
  PixelRGB8 25  10  9   #:
  PixelRGB8 230 170 0   #:
  PixelRGB8 20  40  10  #:
  PixelRGB8 0   100 0   #:
  PixelRGB8 5   10  10  #:
  PixelRGB8 210 70  30  #:
  PixelRGB8 90  0   50  #:
  PixelRGB8 180 90  120 #:
  PixelRGB8 0   20  40  #~
  PixelRGB8 30  70  200

colors2 :: [PixelRGB8]
colors2 =
  PixelRGB8 70  0  20 #!
  PixelRGB8 100 0 100 #:
  PixelRGB8 255 0   0 #~
  PixelRGB8 255 200 0

colors3 :: [PixelRGB8]
colors3 =
  PixelRGB8 40  70  10  #!
  PixelRGB8 40  170 10  #:
  PixelRGB8 100 255 70  #~
  PixelRGB8 255 255 255

colors4 :: [PixelRGB8]
colors4 =
  PixelRGB8 0   0   0   #!
  PixelRGB8 0   0   255 #:
  PixelRGB8 0   255 255 #:
  PixelRGB8 255 255 255 #~
  PixelRGB8 0   128 255

colors5 :: [PixelRGB8]
colors5 =
  PixelRGB8 0   0   0   #!
  PixelRGB8 255 255 255 #~
  PixelRGB8 128 128 128
