module Main where

import Fractals.Args
import Fractals.Coloring
import Fractals.Fractal
import Fractals.Utility
import System.Environment

main :: IO ()
main = do
  (fractal, _) <- parseArgs `fmap` getArgs
  putStr $ showFractal fractal

showFractal :: Fractal -> String
showFractal frac = unlines $
  grid (fractalScreen frac) (ascii (fractalIter frac) .: mkFunction frac)
