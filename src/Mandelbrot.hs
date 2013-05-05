module Main where

import Fractals.Area
import Fractals.Definitions
import Fractals.Image

main :: IO ()
main = writeFractal (mandelbrot 2) 200 4 area "dist/mandelbrot.png"
  where
    screen@(w, h) = (1920, 1080)
    aspect = fromIntegral w / fromIntegral h

    area = fromRectangle
      (-4.3 / 2.0, 4.3 / aspect / 2.0)
      (4.3, 4.3 / aspect)
      screen
