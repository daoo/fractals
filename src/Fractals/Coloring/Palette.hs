{-# LANGUAGE BangPatterns #-}
module Fractals.Coloring.Palette
  ( ColorMap
  , PackedRGBA
  , mkColorMap
  , unsafeColorRgba
  , colors1
  , colors2
  , colors3
  , colors4
  , colors5
  ) where

import Data.Bits
import Data.Vector.Unboxed as V (Vector, fromList, length, unsafeIndex)
import Data.Word
import Fractals.Lerp
import GHC.Base

type RGB = (Word8, Word8, Word8)
type PackedRGBA = Word32

type ColorMap packed = Vector packed

{-# INLINE unsafeColorRgba #-}
unsafeColorRgba :: Vector PackedRGBA -> Int -> Int -> PackedRGBA
unsafeColorRgba !v !m !i = unsafeIndex v ((i `remInt` m) `remInt` V.length v)

mkColorMap :: [PackedRGBA] -> Vector PackedRGBA
mkColorMap = fromList

{-# INLINE pack #-}
pack :: RGB -> PackedRGBA
pack (r, g, b) = r' .|. g' .|. b' .|. a'
  where
    r' = fromIntegral r
    g' = fromIntegral g `unsafeShiftL` 8
    b' = fromIntegral b `unsafeShiftL` 16
    a' = 255            `unsafeShiftL` 24

{-# INLINE interpolate #-}
interpolate :: RGB -> RGB -> [PackedRGBA]
interpolate (r1, g1, b1) (r2, g2, b2) =
  [ f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7, f 8, f 9, f 10, f 11 ]

  where
    s = 11

    f i     = pack (g r1 r2 i, g g1 g2 i, g b1 b2 i)
    g c1 c2 = fromIntegral . unsafeLerpWord s (fromIntegral c1, fromIntegral c2)

{-# INLINE (#!) #-}
(#!) :: RGB -> (RGB -> (RGB, [PackedRGBA])) -> [PackedRGBA]
c1 #! f = interpolate c1 c2 ++ cs
  where
    (c2, cs) = f c1

{-# INLINE (#:) #-}
(#:) :: RGB -> (RGB -> (RGB, [PackedRGBA])) -> RGB -> (RGB, [PackedRGBA])
ci #: f = \c1 -> let (cj, cs) = f c1 in (ci, interpolate ci cj ++ cs)

{-# INLINE (#~) #-}
(#~) :: RGB -> RGB -> RGB -> (RGB, [PackedRGBA])
cn1 #~ cn2 = \c1 -> (cn1, interpolate cn1 cn2 ++ interpolate cn2 c1)

infixr 2 #!
infixr 3 #:
infixr 4 #~

colors1 :: [PackedRGBA]
colors1 =
  (0,   10,  20)  #!
  (50,  100, 240) #:
  (20,  3,   26)  #:
  (230, 60,  20)  #:
  (25,  10,  9)   #:
  (230, 170, 0)   #:
  (20,  40,  10)  #:
  (0,   100, 0)   #:
  (5,   10,  10)  #:
  (210, 70,  30)  #:
  (90,  0,   50)  #:
  (180, 90,  120) #:
  (0,   20,  40)  #~
  (30,  70,  200)

colors2 :: [PackedRGBA]
colors2 =
  (70,  0,  20) #!
  (100, 0, 100) #:
  (255, 0,   0) #~
  (255, 200, 0)

colors3 :: [PackedRGBA]
colors3 =
  (40,  70,  10)  #!
  (40,  170, 10)  #:
  (100, 255, 70)  #~
  (255, 255, 255)

colors4 :: [PackedRGBA]
colors4 =
  (0,   0,   0)   #!
  (0,   0,   255) #:
  (0,   255, 255) #:
  (255, 255, 255) #~
  (0,   128, 255)

colors5 :: [PackedRGBA]
colors5 =
  (0,   0,   0)   #!
  (255, 255, 255) #~
  (128, 128, 128)
