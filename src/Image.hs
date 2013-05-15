module Main where

import Codec.Image.DevIL
import Data.Array.IO (IOUArray)
import Data.Array.Unsafe
import Fractals.Args
import Fractals.Coloring
import Fractals.Image
import Fractals.Utility
import System.Environment

main :: IO ()
main = do
  ilInit
  (f, [img]) <- parseFractal `fmap` getArgs
  Image arr <- call (create (toRgba `xy` greyscale)) f :: IO (Image RGBA IOUArray (Int, Int, Int) Word8)
  unsafeFreeze arr >>= writeImage img
