{-# LANGUAGE BangPatterns #-}
module Fractals.Coloring
  ( ascii
  , greyscale
  , paletted
  , palette1
  , palette2
  , palette3
  , palette4
  , palette5
  ) where

import Codec.Picture.Types
import Data.Word
import Fractals.Definitions (Iterations)
import Fractals.Utility

{-# INLINE ascii #-}
-- |Find a ASCII character representation of a number of iterations.
ascii :: Iterations -- ^Maximum number of iterations
      -> Iterations -- ^Number of iterations for some pixel
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
  _  -> undefined

{-# INLINE greyscale #-}
-- |Find a single channel 8-bit color representation of a number of iterations.
greyscale :: Iterations -- ^Maximum number of iterations
          -> Iterations -- ^Number of iterations for som pixel
          -> Pixel8
greyscale m i = fromIntegral $ scale m 255 i

{-# LANGUAGE paletted #-}
paletted :: [PixelRGB8] -> Iterations -> Iterations -> PixelRGB8
paletted p _ i = p !! (i `rem` (length p))

{-# INLINE interpolate #-}
interpolate :: [PixelRGB8] -> [PixelRGB8]
interpolate colors = go colors
  where
    first = head colors

    go []         = []
    go [c1]       = inner c1 first (0 :: Word)
    go (c1:c2:cs) = inner c1 c2    0 ++ go (c2:cs)

    inner c1 c2 !j
      | j < 12    = color c1 c2 (11 - j) j : inner c1 c2 (j+1)
      | otherwise = []

    color (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) a b =
      PixelRGB8 (f r1 r2) (f g1 g2) (f b1 b2)

      where
        f :: Pixel8 -> Pixel8 -> Pixel8
        f c1 c2 = fromIntegral $ (c1'*a + c2'*b) `div` 11
          where
            c1', c2' :: Word
            c1' = fromIntegral c1
            c2' = fromIntegral c2

{-# INLINE palette1 #-}
{-# INLINE palette2 #-}
{-# INLINE palette3 #-}
{-# INLINE palette4 #-}
{-# INLINE palette5 #-}
palette1, palette2, palette3, palette4, palette5 :: [PixelRGB8]
palette1 = interpolate [ PixelRGB8 0 10 20  , PixelRGB8 50 100 240  , PixelRGB8 20 3 26       , PixelRGB8 230 60 20     , PixelRGB8 25 10 9     , PixelRGB8 230 170 0 , PixelRGB8 20 40 10 , PixelRGB8 0 100 0 , PixelRGB8 5 10 10 , PixelRGB8 210 70 30 , PixelRGB8 90 0 50 , PixelRGB8 180 90 120 , PixelRGB8 0 20 40 , PixelRGB8 30 70 200 ]
palette2 = interpolate [ PixelRGB8 70 0 20  , PixelRGB8 100 0 100   , PixelRGB8 255 0 0       , PixelRGB8 255 200 0 ]
palette3 = interpolate [ PixelRGB8 40 70 10 , PixelRGB8 40 170 10   , PixelRGB8 100 255 70    , PixelRGB8 255 255 255 ]
palette4 = interpolate [ PixelRGB8 0 0 0    , PixelRGB8 0 0 255     , PixelRGB8 0 255 255     , PixelRGB8 255 255 255   , PixelRGB8 0 128 255 ]
palette5 = interpolate [ PixelRGB8 0 0 0    , PixelRGB8 255 255 255 , PixelRGB8 128 128 128 ]
