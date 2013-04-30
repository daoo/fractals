{-# LANGUAGE BangPatterns #-}
module Fractals.Generator
  ( generate
  ) where

import Fractals.Complex
import Fractals.Definitions

generate :: Double -> Int -> Fractal -> Area -> [[Int]]
generate maxAbs maxIter fractal area = goy (complexY area) 0
  where
    iters = countIterations maxAbs (maxIter - 1) fractal

    goy :: Double -> Int -> [[Int]]
    goy !imag !y | y < imageHeight area = gox (complexX area) 0 : goy (imag + complexDY area) (y + 1)
                 | otherwise            = []
      where
        gox :: Double -> Int -> [Int]
        gox !real !x | x < imageWidth area = iters (real :+ imag) : gox (real + complexDX area) (x + 1)
                     | otherwise           = []
