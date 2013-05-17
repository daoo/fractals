module Main where

import Criterion.Main
import Data.Array.IO (IOUArray)
import Data.Word
import Foreign.Marshal.Alloc
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Image

area :: Area
area = aspectCentered (800, 600) 4.3 (-2.0:+0)

iter :: Int
iter = 100

maxabs :: R
maxabs = 4

{-# INLINE array #-}
array :: Color c => (Int -> Int -> c) -> Definition -> IO (IOUArray (Int, Int, Int) Word8)
array color def = do
  arr <- newRgbaArray (areaScreen area)
  fillRgbaArray color def iter maxabs area arr
  return arr

{-# INLINE ptr #-}
ptr :: Color c => (Int -> Int -> c) -> Definition -> IO ()
ptr color def = do
  p <- newRgbaPtr (areaScreen area)
  fillRgbaPtr color def iter maxabs area p
  free p

main :: IO ()
main = defaultMain
  [ bgroup "burningship"
    [ bench "IOUArray" $ array greyscale burningShip
    , bench "Ptr"      $ ptr greyscale burningShip
    ]
  , bgroup "mandelbrot 3"
    [ bench "IOUArray" $ array greyscale (mandelbrot 3)
    , bench "Ptr"      $ ptr greyscale (mandelbrot 3)
    ]
  , bgroup "mandelbrot2"
    [ bench "IOUArray" $ array greyscale mandelbrot2
    , bench "Ptr"      $ ptr greyscale mandelbrot2
    ]
  , bgroup "mandelbrot3"
    [ bench "IOUArray" $ array greyscale mandelbrot3
    , bench "Ptr"      $ ptr greyscale mandelbrot3
    ]
  , bgroup "mandelbrot 2"
    [ bench "IOUArray" $ array greyscale (mandelbrot 2)
    , bench "Ptr"      $ ptr greyscale (mandelbrot 2)
    ]
  ]
