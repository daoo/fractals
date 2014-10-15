-- |Provide parsing of command line arguments.
module Fractals.Args
  ( Fractal(fracDef, fracIter, fracAbs, fracArea)
  , exMandelbrot
  , parseFractal
  , usage
  ) where

import Control.Applicative
import Fractals.Area
import Fractals.Complex
import Fractals.Definitions
import Fractals.Math
import Text.Read

data Fractal = Fractal
  { fracDef :: Definition
  , fracIter :: Int
  , fracAbs :: R
  , fracArea :: Area
  }

exMandelbrot :: Fractal
exMandelbrot = Fractal mandelbrot2 200 4 (fromAspectCentered (mkSize 1920 1080) 4.3 (0:+0))

readMaybeSize :: String -> String -> Maybe Size
readMaybeSize w h = mkSize <$> readMaybe w <*> readMaybe h

readMaybeComp :: String -> String -> Maybe (Complex R)
readMaybeComp r c = (:+) <$> readMaybe r <*> readMaybe c

readMaybeArea :: String -> String -> String -> String -> String -> Maybe Area
readMaybeArea w h pw x0 y0 =
  fromAspectCentered <$> readMaybeSize w h <*> readMaybe pw <*> readMaybeComp x0 y0

parseFractal :: [String] -> Maybe Fractal
parseFractal ["mandelbrot", "2", i, w, h, pw, x0, y0] =
  Fractal mandelbrot2 <$> readMaybe i <*> pure 4 <*> readMaybeArea w h pw x0 y0

parseFractal ["mandelbrot", "3", i, w, h, pw, x0, y0] =
  Fractal mandelbrot3 <$> readMaybe i <*> pure 4 <*> readMaybeArea w h pw x0 y0

parseFractal ["mandelbrot", power, i, w, h, pw, x0, y0] =
  Fractal <$> (mandelbrot <$> readMaybe power) <*> readMaybe i <*> pure 4 <*> readMaybeArea w h pw x0 y0

parseFractal ["burningship", i, w, h, pw, x0, y0] =
  Fractal burningShip <$> readMaybe i <*> pure 4 <*> readMaybeArea w h pw x0 y0

parseFractal ["julia", x, y, i, w, h, pw, x0, y0] =
  Fractal <$> (julia <$> readMaybeComp x y) <*> readMaybe i <*> pure 4 <*> readMaybeArea w h pw x0 y0

parseFractal _ = Nothing

usage :: String
usage = "FRACTAL [FRACTAL ARGS] MAXITER WIDTH HEIGHT PLANEWIDTH CENTERX CENTERY\n\
        \\n\
        \Availible fractals are:\n\
        \  mandelbrot POWER\n\
        \  burningship\n\
        \  julia X Y"
