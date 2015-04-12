{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- |Provide parsing of command line arguments.
module Fractals.Args
  ( Fractal(fracDef, fracIter, fracAbs, fracArea)
  , exMandelbrot
  , parseFractal
  , usage
  ) where

import Data.Complex
import Fractals.Data.Area
import Fractals.Data.Size
import Fractals.Definitions
import GHC.Base
import Text.Read

data Fractal = Fractal
  { fracDef  :: !Definition
  , fracIter :: !Int
  , fracAbs  :: !Double
  , fracArea :: !Area
  }

maxabs :: Double
maxabs = 4.0

exMandelbrot :: Fractal
exMandelbrot = Fractal mandelbrot2 200 maxabs (fromAspectCentered (mkSize 1920 1080) 4.3 (0:+0))

readMaybeSize :: String -> String -> Maybe Size
readMaybeSize w h = mkSize <$> readMaybe w <*> readMaybe h

readMaybeComp :: String -> String -> Maybe (Complex Double)
readMaybeComp r c = (:+) <$> readMaybe r <*> readMaybe c

readMaybeArea :: String -> String -> String -> String -> String -> Maybe Area
readMaybeArea w h pw x0 y0 =
  fromAspectCentered <$> readMaybeSize w h <*> readMaybe pw <*> readMaybeComp x0 y0

parseFractal :: [String] -> Maybe Fractal
parseFractal ["mandelbrot", "2", i, w, h, pw, x0, y0] =
  Fractal mandelbrot2 <$> readMaybe i <*> pure maxabs <*> readMaybeArea w h pw x0 y0

parseFractal ["mandelbrot", "3", i, w, h, pw, x0, y0] =
  Fractal mandelbrot3 <$> readMaybe i <*> pure maxabs <*> readMaybeArea w h pw x0 y0

parseFractal ["mandelbrot", power, i, w, h, pw, x0, y0] = do
  I# p <- readMaybe power
  maxiter <- readMaybe i
  area <- readMaybeArea w h pw x0 y0
  return $ Fractal (mandelbrot p) maxiter maxabs area

parseFractal ["burningship", i, w, h, pw, x0, y0] =
  Fractal burningShip <$> readMaybe i <*> pure maxabs <*> readMaybeArea w h pw x0 y0

parseFractal ["julia", x, y, i, w, h, pw, x0, y0] = do
  D# x' <- readMaybe x
  D# y' <- readMaybe y
  maxiter <- readMaybe i
  area <- readMaybeArea w h pw x0 y0

  return $ Fractal (julia (# x', y' #)) maxiter maxabs area

parseFractal _ = Nothing

usage :: String
usage = "FRACTAL [FRACTAL ARGS] MAXITER WIDTH HEIGHT PLANEWIDTH CENTERX CENTERY\n\
        \\n\
        \Availible fractals are:\n\
        \  mandelbrot POWER\n\
        \  burningship\n\
        \  julia X Y"
