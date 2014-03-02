module Main where

import Codec.Image.DevIL
import Data.Array.Unsafe
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Geometry
import Fractals.Storage
import Fractals.Utility

main :: IO ()
main = do
  ilInit
  arr <- newRgbaArray (areaScreen area)
  measureTime $ fillRgbaArray arr (toRgba ... greyscale) mandelbrot2 200 4 area
  unsafeFreeze arr >>= writeImage "dist/mandelbrot.png"
  where
    area = fromAspectCentered (Vec 1920 1080) 4.3 (0:+0)
