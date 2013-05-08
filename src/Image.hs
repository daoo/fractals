module Main where

import Codec.Image.DevIL
import Data.Array.Unsafe
import Fractals.Args
import Fractals.Output
import System.Environment

main :: IO ()
main = do
  ilInit
  (f, [img]) <- parseFractal `fmap` getArgs
  a <- array (fractalDefinition f) (fractalIter f) (fractalMaxAbs f) (fractalArea f)
  a' <- unsafeFreeze a
  writeImage img a'
