module Main where

import Fractals.Args
import Fractals.Fractal
import Fractals.Output
import System.Environment

main :: IO ()
main = do
  fractal <- parseArgs `fmap` getArgs
  putStrLn $ showFractal (fractalIter fractal) $ render fractal
