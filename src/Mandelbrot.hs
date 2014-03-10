module Main where

import Codec.Image.DevIL
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
  ptr <- newRgbaPtr (areaScreen area)
  measureTime $ fillRgbaPtr ptr (toRgba ... greyscale) mandelbrot2 200 4 area
  writeImageFromPtr "dist/mandelbrot.png" (w, h) ptr
  where
    w = 1920
    h = 1080

    area = fromAspectCentered (Vec w h) 4.3 (0:+0)
