module Main where

import Codec.Image.DevIL
import Data.Array.Unsafe
import Fractals.Args
import Fractals.Output
import System.Environment

main :: IO ()
main = ilInit >> parseFractal `fmap` getArgs >>= \(f, [img]) ->
  call rgbaArray f >>= unsafeFreeze >>= writeImage img
