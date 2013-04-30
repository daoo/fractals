module Fractals.Fractal where

import Fractals.Area
import Fractals.Definitions
import Fractals.Generator

data Fractal = Fractal
  { fractalDef     :: Definition
  , fractalIter    :: Int
  , fractalTopLeft :: (Double, Double)
  , fractalPlane   :: (Double, Double)
  , fractalScreen  :: (Int, Int)
  }

render :: Fractal -> [[Int]]
render (Fractal frac iter topleft plane screen) =
  generate 4.0 iter frac (rectangles screen plane topleft)
