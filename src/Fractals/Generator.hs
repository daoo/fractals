module Fractals.Generator
  ( generate
  ) where

import Fractals.Area
import Fractals.Complex
import Fractals.Definitions
import Fractals.Utility

{-# INLINE calcReal #-}
{-# INLINE calcImag #-}
calcReal, calcImag :: Area -> Int -> R
calcReal area x = planeMinX area + fromIntegral x * planeDeltaX area
calcImag area y = planeMinY area + fromIntegral y * planeDeltaY area

{-# INLINE screenToPlane #-}
screenToPlane :: Area -> Int -> Int -> Comp
screenToPlane area x y = calcReal area x :+ calcImag area y

generate :: R -> Int -> Definition -> Area -> [[Int]]
generate maxAbs maxIter fractal area = grid
  (imageWidth area, imageHeight area)
  (\x y -> fractal (screenToPlane area x y) maxAbs maxIter)
