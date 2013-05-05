module Main where

import Fractals.Area
import Fractals.Args
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Utility
import System.Environment

main :: IO ()
main = do
  (f, _) <- parseArgs `fmap` getArgs
  putStr $ showFractal (fractalDefinition f) (fractalIter f) (fractalMaxAbs f) (fractalArea f)

{-# INLINE showFractal #-}
showFractal :: Definition -> Int -> R -> Area -> String
showFractal fractal iter maxabs (Area topleft _ delta screen) =
  unlines $ grid screen topleft delta (ascii iter .: func)
  where
    func x y = fractal (x :+ y) maxabs iter
