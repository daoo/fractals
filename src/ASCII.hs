module Main where

import Fractals.Args
import Fractals.Render
import System.Environment

main :: IO ()
main = do
  (Fractal def iter maxabs area, _) <- parseFractal `fmap` getArgs
  putStr $ string def iter maxabs area
