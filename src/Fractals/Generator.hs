module Fractals.Generator
  ( generate
  ) where

import Fractals.Area
import Fractals.Complex
import Fractals.Definitions
import Fractals.Utility

generate :: Double -> Int -> Definition -> Area -> [[Int]]
generate maxAbs maxIter fractal area = grid
  (imageWidth area, imageHeight area)
  (complexX area, complexY area)
  (complexDX area, complexDY area)
  (\a b -> countIterations maxAbs (maxIter - 1) fractal (a :+ b))
