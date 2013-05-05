module Main where

import Fractals.Definitions
import Fractals.Fractal
import Fractals.Output

main :: IO ()
main = writeFractal (fractalIter fractal) "dist/mandelbrot.png" $ render fractal
  where
    screen@(w, h) = (1920, 1080)
    aspect = fromIntegral w / fromIntegral h

    fractal = Fractal
      (mandelbrot 2)
      200
      4
      (-4.3 / 2.0, 4.3 / aspect / 2.0)
      (4.3, 4.3 / aspect)
      screen
