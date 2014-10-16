{-# LANGUAGE BangPatterns #-}
module Fractals.Coloring
  ( ascii
  , greyscale
  , interpolate
  , optimizeStorage
  , paletted
  , palette1
  , palette2
  , palette3
  , palette4
  , palette5
  ) where

import Codec.Picture.Types
import Data.Vector.Storable as V (Vector, fromList, length)
import Data.Word
import Fractals.Math
import GHC.Base

{-# INLINE ascii #-}
-- |Find a ASCII character representation of a number of iterations.
ascii :: Int -- ^Maximum number of iterations
      -> Int -- ^Number of iterations for some pixel
      -> Char
ascii m i = case scale m 10 i of
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
greyscale m i = fromIntegral $ scale m 255 i

{-# INLINE paletted #-}
paletted :: Vector (PixelBaseComponent PixelRGB8) -> Int -> Int -> PixelRGB8
paletted p m i
  | i == 0 || m == i = unsafePixelAt p 0
  | otherwise        = p `unsafePixelAt` ((i*3) `remInt` V.length p)

optimizeStorage :: [PixelRGB8] -> Vector (PixelBaseComponent PixelRGB8)
optimizeStorage = fromList . concatMap (\(PixelRGB8 r g b) -> [r,g,b])

interpolate :: [PixelRGB8] -> [PixelRGB8]
interpolate colors = go colors
  where
    first = head colors

    steps = 12

    go []         = []
    go [c1]       = map (lerpRgb8 steps (c1, first)) [0..steps-1]
    go (c1:c2:cs) = map (lerpRgb8 (steps-1) (c1, c2)) [0..steps-1] ++ go (c2:cs)

{-# INLINE lerpRgb8 #-}
lerpRgb8 :: Word -> (PixelRGB8, PixelRGB8) -> Word -> PixelRGB8
lerpRgb8 s ((PixelRGB8 !r1 !g1 !b1), (PixelRGB8 !r2 !g2 !b2)) i =
  PixelRGB8 (g r1 r2) (g g1 g2) (g b1 b2)
  where
    g c1 c2 = fromIntegral $ lerpw s (fromIntegral c1, fromIntegral c2) i

{-# INLINE palette1 #-}
palette1 :: [PixelRGB8]
palette1 =
  [ PixelRGB8 0 10 20
  , PixelRGB8 50 100 240
  , PixelRGB8 20 3 26
  , PixelRGB8 230 60 20
  , PixelRGB8 25 10 9
  , PixelRGB8 230 170 0
  , PixelRGB8 20 40 10
  , PixelRGB8 0 100 0
  , PixelRGB8 5 10 10
  , PixelRGB8 210 70 30
  , PixelRGB8 90 0 50
  , PixelRGB8 180 90 120
  , PixelRGB8 0 20 40
  , PixelRGB8 30 70 200
  ]

{-# INLINE palette2 #-}
palette2 :: [PixelRGB8]
palette2 =
  [ PixelRGB8 70 0 20
  , PixelRGB8 100 0 100
  , PixelRGB8 255 0 0
  , PixelRGB8 255 200 0
  ]

{-# INLINE palette3 #-}
palette3 :: [PixelRGB8]
palette3 =
  [ PixelRGB8 40 70 10
  , PixelRGB8 40 170 10
  , PixelRGB8 100 255 70
  , PixelRGB8 255 255 255
  ]

{-# INLINE palette4 #-}
palette4 :: [PixelRGB8]
palette4 =
  [ PixelRGB8 0 0 0
  , PixelRGB8 0 0 255
  , PixelRGB8 0 255 255
  , PixelRGB8 255 255 255
  , PixelRGB8 0 128 255
  ]

{-# INLINE palette5 #-}
palette5 :: [PixelRGB8]
palette5 =
  [ PixelRGB8 0 0 0
  , PixelRGB8 255 255 255
  , PixelRGB8 128 128 128
  ]
