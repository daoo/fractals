module Main where

import Fractals.Area
import Fractals.Definitions
import Fractals.Show

main :: IO ()
main = putStr $ showFractal (mandelbrot 2) 200 4 area
  where
    screen@(w, h) = (1920, 1080)
    aspect = realToFrac w / realToFrac h

    area = fromRectangle
      screen
      (-4.3 / 2.0, 4.3 / aspect / 2.0)
      (4.3, 4.3 / aspect)
