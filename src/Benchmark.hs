module Main where

import Criterion.Main
import Data.Array.IO
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

{-# INLINE create' #-}
create' :: ImageArray c a i e m => (Int -> Int -> c) -> Definition -> m (Image c a i e)
create' color def = create color def 100 4 area

main :: IO ()
main = defaultMain
  [ bgroup "rgba"
    [ bgroup "IOUArray"
      [ bench "mandelbrot 2" $ (create' (toRgba `xy` greyscale) (mandelbrot 2) :: IO (Image RGBA IOUArray (Int, Int, Int) Word8))
      , bench "mandelbrot2"  $ (create' (toRgba `xy` greyscale) mandelbrot2    :: IO (Image RGBA IOUArray (Int, Int, Int) Word8))
      , bench "mandelbrot 3" $ (create' (toRgba `xy` greyscale) (mandelbrot 3) :: IO (Image RGBA IOUArray (Int, Int, Int) Word8))
      , bench "mandelbrot3"  $ (create' (toRgba `xy` greyscale) mandelbrot3    :: IO (Image RGBA IOUArray (Int, Int, Int) Word8))
      , bench "burningship"  $ (create' (toRgba `xy` greyscale) burningShip    :: IO (Image RGBA IOUArray (Int, Int, Int) Word8))
      ]
    ]
  ]
