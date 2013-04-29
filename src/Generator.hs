{-# LANGUAGE BangPatterns #-}
module Generator where

import Complex
import Fractals

{-# INLINE countIterations #-}
countIterations :: Double -> Int -> FractalFunc -> Comp -> Int
countIterations maxAbs maxIter fractal c = go 0 (0 :+ 0)
  where
    go !i !z | i >= maxIter || magSquared z >= maxAbs = i
             | otherwise                              = go (i + 1) (fractal c z)

generate :: Double -> Int -> FractalFunc -> Comp -> Comp -> (Int, Int) -> [[Int]]
generate maxAbs maxIter fractal (sr :+ si) (dr :+ di) (w, h) = goy si 0
  where
    iters = countIterations maxAbs (maxIter - 1) fractal

    goy :: Double -> Int -> [[Int]]
    goy !imag !y | y < h     = gox sr 0 : goy (imag + di) (y + 1)
                 | otherwise = []
      where
        gox :: Double -> Int -> [Int]
        gox !real !x | x < w     = iters (real :+ imag) : gox (real + dr) (x + 1)
                     | otherwise = []
