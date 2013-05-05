module Main where

import Fractals.Args
import Fractals.Fractal
import Fractals.Output
import System.Environment

main :: IO ()
main = do
  (fractal, [img]) <- parseArgs `fmap` getArgs
  writeFractal (fractalIter fractal) img $ render fractal
