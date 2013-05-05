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
showFractal fractal iter maxabs (Area (px, py) _ (dx, dy) screen) =
  unlines $ grid screen (ascii iter .: func)
  where
    func x y = fractal (screenToPlane x y) maxabs iter

    real x = px + fromIntegral x * dx
    imag y = py + fromIntegral y * dy

    screenToPlane x y = real x :+ imag y
