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
import Fractals.Geometry
import Text.Read

data Fractal = Fractal
  { fracDef :: Definition
  , fracIter :: Int
  , fracAbs :: R
  , fracArea :: Area
  }

exMandelbrot :: Fractal
exMandelbrot = Fractal mandelbrot2 200 4 (fromAspectCentered (Vec 1920 1080) 4.3 (0:+0))

{-# INLINE readMaybeVec #-}
readMaybeVec :: String -> String -> Maybe Vec
readMaybeVec x y = Vec <$> readMaybe x <*> readMaybe y

{-# INLINE readMaybeComp #-}
readMaybeComp :: String -> String -> Maybe Comp
readMaybeComp r c = (:+) <$> readMaybe r <*> readMaybe c

{-# INLINE readMaybeArea #-}
readMaybeArea :: String -> String -> String -> String -> String -> String -> Maybe Area
readMaybeArea w h pw ph x0 y0 =
  fromRectangle <$> readMaybeVec w h <*> readMaybeComp pw ph <*> readMaybeComp x0 y0

parseFractal :: [String] -> Maybe Fractal
parseFractal ["mandelbrot", "2", i, w, h, pw, ph, x0, y0] =
  Fractal mandelbrot2 <$> readMaybe i <*> pure 4 <*> readMaybeArea w h pw ph x0 y0

parseFractal ["mandelbrot", "3", i, w, h, pw, ph, x0, y0] =
  Fractal mandelbrot3 <$> readMaybe i <*> pure 4 <*> readMaybeArea w h pw ph x0 y0

parseFractal ["mandelbrot", power, i, w, h, pw, ph, x0, y0] =
  Fractal <$> (mandelbrot <$> readMaybe power) <*> readMaybe i <*> pure 4 <*> readMaybeArea w h pw ph x0 y0

parseFractal ["burningship", i, w, h, pw, ph, x0, y0] =
  Fractal burningShip <$> readMaybe i <*> pure 4 <*> readMaybeArea w h pw ph x0 y0

parseFractal ["julia", x, y, i, w, h, pw, ph, x0, y0] =
  Fractal <$> (julia <$> readMaybeComp x y) <*> readMaybe i <*> pure 4 <*> readMaybeArea w h pw ph x0 y0

parseFractal _ = Nothing

usage :: String
usage = "FRACTAL [FRACTAL ARGS] MAXITER WIDTH HEIGHT PLANEWIDTH PLANEHEIGHT TOPLEFTX TOPLEFTY\n\
        \\n\
        \Availible fractals are:\n\
        \  mandelbrot POWER\n\
        \  burningship\n\
        \  julia X Y"
