module Main where

import Fractals.Args
import Fractals.Output
import System.Environment

main :: IO ()
main = do
  (f, _) <- parseFractal `fmap` getArgs
  putStr $ string (fractalDefinition f) (fractalIter f) (fractalMaxAbs f) (fractalArea f)
