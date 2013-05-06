{-# LANGUAGE CPP #-}
module Main where

import Fractals.Area
import Fractals.Args
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Utility
import System.Environment

{-# INLINE parse #-}
parse :: [String] -> (Fractal, [String])
#ifdef MANDELBROT2
parse = parseFractal1 (return mandelbrot2')
#elif MANDELBROT
parse = parseFractal1 parseMandelbrot
#elif JULIA
parse = parseFractal1 parseJulia
#elif BURNINGSHIP
parse = parseFractal1 (return burningShip)
#else
parse = parseFractal
#endif

main :: IO ()
main = do
  (f, _) <- parse `fmap` getArgs
  putStr $ showFractal (fractalDefinition f) (fractalIter f) (fractalMaxAbs f) (fractalArea f)

{-# INLINE showFractal #-}
showFractal :: Definition -> Int -> R -> Area -> String
showFractal fractal iter maxabs (Area topleft _ delta screen) =
  unlines $ grid screen topleft delta (ascii iter .: func)
  where
    func x y = fractal (x :+ y) maxabs iter
