module Main where

import Fractals.Args
import Fractals.Coloring
import Fractals.Complex
import Fractals.Fractal
import Fractals.Utility
import System.Environment

main :: IO ()
main = do
  (fractal, _) <- parseArgs `fmap` getArgs
  putStr $ showFractal fractal

showFractal :: Fractal -> String
showFractal fractal = unlines $ grid screen (ascii i .: func)
  where
    func x y = fractalDef fractal (screenToPlane x y) a i

    i = fractalIter fractal
    a = fractalMaxAbs fractal

    screen   = fractalScreen fractal
    (px, py) = fractalTopLeft fractal
    (dx, dy) = fractalDelta fractal

    real x = px + fromIntegral x * dx
    imag y = py + fromIntegral y * dy

    screenToPlane x y = real x :+ imag y
