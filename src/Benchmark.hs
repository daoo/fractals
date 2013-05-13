module Main where

import Control.Monad
import Criterion.Main
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Render
import Fractals.Utility

{-# INLINE test #-}
test :: Definition -> IO ()
test def = void $ rgbaArray (greyscaleToRgba `xy` greyscale) def 100 4 area
  where
    screen@(w, h) = (1000, 1000)
    aspect = realToFrac w / realToFrac h

    area = aspectCentered (1920, 1080) 4.3 (-2.0:+0)

main :: IO ()
main = defaultMain
  [ bench "mandelbrot 2" $ test (mandelbrot 2)
  , bench "mandelbrot2"  $ test mandelbrot2
  , bench "mandelbrot 3" $ test (mandelbrot 3)
  , bench "mandelbrot3"  $ test mandelbrot3
  , bench "burningship"  $ test burningShip
  ]
