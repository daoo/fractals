module Main where

import Codec.Image.DevIL
import Data.Array.Unsafe
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Render
import Fractals.Utility

main :: IO ()
main = do
  ilInit
  arr <- rgbaArray (greyscaleToRgba `xy` greyscale) mandelbrot2 200 4 area
  unsafeFreeze arr >>= writeImage "dist/mandelbrot.png"
  where
    area = aspectCentered (1920, 1080) 4.3 (-2.0:+0)
