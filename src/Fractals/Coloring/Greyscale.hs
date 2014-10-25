module Fractals.Coloring.Greyscale where

import Data.Word
import Fractals.Math

{-# INLINE greyscale #-}
-- |Find a single channel 8-bit color representation of a number of iterations.
greyscale :: Int -- ^Maximum number of iterations
          -> Int -- ^Number of iterations for som pixel
          -> Word8
greyscale m i = fromIntegral $ unsafeScaleInt m 255 i
