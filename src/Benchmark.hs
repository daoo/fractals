module Main where

import Criterion.Main
import Data.Array.IO (IOUArray)
import Data.Word
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Image

{-# INLINE area #-}
area :: Area
area = aspectCentered (800, 600) 4.3 (-2.0:+0)

{-# INLINE create #-}
create :: Color c => (Int -> Int -> c) -> Definition -> IO (IOUArray (Int, Int, Int) Word8)
create color def = do
  arr <- newRgbaArray (areaScreen area)
  fillRgbaArray color def 100 4 area arr
  return arr

main :: IO ()
main = defaultMain
  [ bgroup "rgba"
    [ bgroup "IOUArray"
      [ bench "mandelbrot 2" $ create greyscale (mandelbrot 2)
      , bench "mandelbrot2"  $ create greyscale mandelbrot2
      , bench "mandelbrot 3" $ create greyscale (mandelbrot 3)
      , bench "mandelbrot3"  $ create greyscale mandelbrot3
      , bench "burningship"  $ create greyscale burningShip
      ]
    ]
  ]
