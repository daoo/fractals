module Main where

import Control.Monad
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
area = aspectCentered (1000, 1000) 4.3 (-2.0:+0)

{-# INLINE test #-}
test :: Definition -> IO ()
test def = void $ (create (toRgba `xy` greyscale) def 100 4 area :: IO RgbaImage)

{-# INLINE test2 #-}
test2 :: Definition -> IO ()
test2 def = void $ (create greyscale def 100 4 area :: IO (Image Greyscale IOUArray (Int, Int) Word8))

main :: IO ()
main = defaultMain
  [ bgroup "rgba"
    [ bench "mandelbrot 2" $ test (mandelbrot 2)
    , bench "mandelbrot2"  $ test mandelbrot2
    , bench "mandelbrot 3" $ test (mandelbrot 3)
    , bench "mandelbrot3"  $ test mandelbrot3
    , bench "burningship"  $ test burningShip
    ]
  , bgroup "greyscale"
    [ bench "mandelbrot2" $ test2 mandelbrot2
    ]
  ]
