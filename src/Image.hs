module Main where

import Fractals.Args
import Fractals.Image
import System.Environment

main :: IO ()
main = do
  (f, [img]) <- parseArgs `fmap` getArgs
  writeFractal (fractalDefinition f) (fractalIter f) (fractalMaxAbs f) (fractalArea f) img
