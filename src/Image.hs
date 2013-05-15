module Main where

import Codec.Image.DevIL
import Data.Array.IO (IOUArray)
import Data.Array.Unsafe
import Fractals.Area
import Fractals.Args
import Fractals.Coloring
import Fractals.Image
import Fractals.Utility
import System.Environment

main :: IO ()
main = do
  ilInit
  (Fractal def iter maxabs area, [path]) <- parseFractal `fmap` getArgs
  img@(ArrayImage arr) <- new (areaScreen area) :: IO (ArrayImage IOUArray (Int, Int, Int) RGBA Word8)
  fill (toRgba `xy` greyscale) def iter maxabs area img
  unsafeFreeze arr >>= writeImage path
