module Main (main) where

import Fractals.Args
import Fractals.Render
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case parseFractal args of
    Nothing -> putStrLn usage
    Just f  -> putStr $ string (fracDef f) (fracIter f) (fracAbs f) (fracArea f)
