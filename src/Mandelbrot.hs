module Main where

import Data.Array.IO (IOUArray)
import Data.Array.Unsafe
import Data.Word
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Geometry
import Fractals.Image
import Fractals.PNG
import Fractals.Utility

main :: IO ()
main = do
  arr <- newGreyscaleArray (areaScreen area) :: IO (IOUArray (Int, Int) Word8)
  measureTime $ fill arr greyscale mandelbrot2 200 4 area
  png "dist/mandelbrot.png" (w, h) arr
  where
    w = 1920
    h = 1080

    area = fromAspectCentered (Vec w h) 4.3 (0:+0)
