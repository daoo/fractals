module Main where

import Criterion.Main
import Data.Array.IO (IOUArray)
import Data.Word
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Image
import Fractals.Utility

{-# INLINE area #-}
area :: Area
area = aspectCentered (800, 600) 4.3 (-2.0:+0)

{-# INLINE create #-}
create :: ImageArray c e i m => (Int -> Int -> c) -> Definition -> m (i c e)
create color def = do
  img <- new (areaScreen area)
  fill color def 100 4 area img
  return img

type RgbaArrayImage a = ArrayImage a (Int, Int, Int) RGBA Word8

main :: IO ()
main = defaultMain
  [ bgroup "rgba"
    [ bgroup "IOUArray"
      [ bench "mandelbrot 2" $ (create (toRgba `xy` greyscale) (mandelbrot 2) :: IO (RgbaArrayImage IOUArray))
      , bench "mandelbrot2"  $ (create (toRgba `xy` greyscale) mandelbrot2    :: IO (RgbaArrayImage IOUArray))
      , bench "mandelbrot 3" $ (create (toRgba `xy` greyscale) (mandelbrot 3) :: IO (RgbaArrayImage IOUArray))
      , bench "mandelbrot3"  $ (create (toRgba `xy` greyscale) mandelbrot3    :: IO (RgbaArrayImage IOUArray))
      , bench "burningship"  $ (create (toRgba `xy` greyscale) burningShip    :: IO (RgbaArrayImage IOUArray))
      ]
    ]
  ]
