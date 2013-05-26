module Main where

import Codec.Image.DevIL
import Data.Array.IO (IOUArray)
import Data.Array.Unsafe
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Image
import Fractals.Utility

main :: IO ()
main = do
  ilInit
  arr <- newRgbaArray (areaScreen area) :: IO (IOUArray (Int, Int, Int) Word8)
  measureTime $ fill arr (toRgba `xy` greyscale) mandelbrot2 200 4 area
  unsafeFreeze arr >>= writeImage "dist/mandelbrot.png"
  where
    area = fromAspectCentered (1920, 1080) 4.3 (0:+0)
