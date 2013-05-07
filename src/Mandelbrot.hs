module Main where

import Fractals.Area
import Fractals.Definitions
import Fractals.Image

main :: IO ()
main = writeFractal mandelbrot2' 200 4 area "dist/mandelbrot.png"
  where
    screen@(w, h) = (1920, 1080)
    aspect = realToFrac w / realToFrac h

    area = fromRectangle
      screen
      (-4.3 / 2.0, 4.3 / aspect / 2.0)
      (4.3, 4.3 / aspect)
