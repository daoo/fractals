module Main where

import Control.Monad
import Criterion.Main
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Image
import Fractals.Utility

{-# INLINE test #-}
test :: Definition -> IO ()
test def = void $ (create (toRgba `xy` greyscale) def 100 4 area :: IO RgbaImage)
  where
    area = aspectCentered (1000, 1000) 4.3 (-2.0:+0)

main :: IO ()
main = defaultMain
  [ bench "mandelbrot 2" $ test (mandelbrot 2)
  , bench "mandelbrot2"  $ test mandelbrot2
  , bench "mandelbrot 3" $ test (mandelbrot 3)
  , bench "mandelbrot3"  $ test mandelbrot3
  , bench "burningship"  $ test burningShip
  ]
