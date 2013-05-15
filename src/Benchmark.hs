module Main where

import Criterion.Main
import Data.Array.IO (IOUArray)
import Data.Array.Storable (StorableArray)
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

type RgbaImage a = Image RGBA a (Int, Int, Int) Word8

main :: IO ()
main = defaultMain
  [ bgroup "rgba"
    [ bgroup "IOUArray"
      [ bench "mandelbrot 2" $ (create' (toRgba `xy` greyscale) (mandelbrot 2) :: IO (RgbaImage IOUArray))
      , bench "mandelbrot2"  $ (create' (toRgba `xy` greyscale) mandelbrot2    :: IO (RgbaImage IOUArray))
      , bench "mandelbrot 3" $ (create' (toRgba `xy` greyscale) (mandelbrot 3) :: IO (RgbaImage IOUArray))
      , bench "mandelbrot3"  $ (create' (toRgba `xy` greyscale) mandelbrot3    :: IO (RgbaImage IOUArray))
      , bench "burningship"  $ (create' (toRgba `xy` greyscale) burningShip    :: IO (RgbaImage IOUArray))
      ]
    , bgroup "StorableArray"
      [ bench "mandelbrot 2" $ (create' (toRgba `xy` greyscale) (mandelbrot 2) :: IO (RgbaImage StorableArray))
      , bench "mandelbrot2"  $ (create' (toRgba `xy` greyscale) mandelbrot2    :: IO (RgbaImage StorableArray))
      , bench "mandelbrot 3" $ (create' (toRgba `xy` greyscale) (mandelbrot 3) :: IO (RgbaImage StorableArray))
      , bench "mandelbrot3"  $ (create' (toRgba `xy` greyscale) mandelbrot3    :: IO (RgbaImage StorableArray))
      , bench "burningship"  $ (create' (toRgba `xy` greyscale) burningShip    :: IO (RgbaImage StorableArray))
      ]
    ]
  ]
