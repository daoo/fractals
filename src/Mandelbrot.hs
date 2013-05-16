module Main where

import Codec.Image.DevIL
import Data.Array.IO (IOUArray)
import Data.Array.Unsafe
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Image

main :: IO ()
main = do
  ilInit
  arr <- newRgbaArray (areaScreen area) :: IO (IOUArray (Int, Int, Int) Word8)
  fillRgbaArray greyscale mandelbrot2 200 4 area arr
  unsafeFreeze arr >>= writeImage "dist/mandelbrot.png"
  where
    area = aspectCentered (1920, 1080) 4.3 (-2.0:+0)
