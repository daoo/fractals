module Fractals.Fractal where

import Fractals.Area
import Fractals.Complex
import Fractals.Definitions
import Fractals.Generator

data Fractal = Fractal
  { fractalDef     :: Definition
  , fractalIter    :: Int
  , fractalTopLeft :: (R, R)
  , fractalPlane   :: (R, R)
  , fractalScreen  :: (Int, Int)
  }

render :: Fractal -> [[Int]]
render (Fractal frac iter topleft plane screen) =
  generate (fromInteger 4) iter frac (rectangles screen plane topleft)
