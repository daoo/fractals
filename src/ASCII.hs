module Main where

import Fractals.Args
import Fractals.Show
import System.Environment

main :: IO ()
main = do
  (f, _) <- parseFractal `fmap` getArgs
  putStr $ showFractal (fractalDefinition f) (fractalIter f) (fractalMaxAbs f) (fractalArea f)
