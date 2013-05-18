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
  arr <- newRgbaArray (areaScreen area) :: IO (IOUArray (Int, Int, Int) Word8)
  measureTime $ fill arr greyscale def iter maxabs area
  unsafeFreeze arr >>= writeImage path
