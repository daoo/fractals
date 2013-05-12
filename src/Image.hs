module Main where

import Codec.Image.DevIL
import Data.Array.Unsafe
import Fractals.Args
import Fractals.Coloring
import Fractals.Render
import Fractals.Utility
import System.Environment

main :: IO ()
main = ilInit >> parseFractal `fmap` getArgs >>= \(f, [img]) ->
  call (rgbaArray (greyscaleToRgba `xy` greyscale)) f >>= unsafeFreeze >>= writeImage img
