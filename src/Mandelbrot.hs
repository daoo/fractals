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

type RgbaArrayImage = ArrayImage IOUArray (Int, Int, Int) RGBA Word8

main :: IO ()
main = do
  ilInit
  img@(ArrayImage arr) <- new (areaScreen area):: IO RgbaArrayImage
  fill (toRgba `xy` greyscale) mandelbrot2 200 4 area img
  unsafeFreeze arr >>= writeImage "dist/mandelbrot.png"
  where
    area = aspectCentered (1920, 1080) 4.3 (-2.0:+0)
