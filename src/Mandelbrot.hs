module Main where

import Codec.Image.DevIL
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
  Image arr <- create (toRgba `xy` greyscale) mandelbrot2 200 4 area :: IO RgbaImage
  unsafeFreeze arr >>= writeImage "dist/mandelbrot.png"
  where
    area = aspectCentered (1920, 1080) 4.3 (-2.0:+0)
