module Main where

import Fractals.Args
import Fractals.Image
import System.Environment

main :: IO ()
main = do
  (fractal, [img]) <- parseArgs `fmap` getArgs
  writeFractal fractal img
