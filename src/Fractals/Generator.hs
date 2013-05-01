module Fractals.Generator
  ( generate
  ) where

import Fractals.Area
import Fractals.Complex
import Fractals.Definitions
import Fractals.Utility

generate :: R -> Int -> Definition -> Area -> [[Int]]
generate maxAbs maxIter fractal area = grid
  (imageWidth area, imageHeight area)
  (planeMinX area, planeMinY area)
  (planeDeltaX area, planeDeltaY area)
  (\a b -> countIterations maxAbs (maxIter - 1) fractal (a :+ b))
