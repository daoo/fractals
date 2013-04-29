{-# LANGUAGE BangPatterns #-}
module Fractals.Definitions
  ( Fractal
  , mandelbrot
  , burningShip
  , julia
  , countIterations
  ) where

import Fractals.Complex

type Fractal = Comp -> Comp -> Comp

{-# INLINE mandelbrot #-}
mandelbrot :: Int -> Fractal
mandelbrot 0 c _ = c
mandelbrot 1 c z = z + c
mandelbrot 2 c z = z * z + c
mandelbrot a c z = z ^ a + c

{-# INLINE burningShip #-}
burningShip :: Fractal
burningShip (r :+ i) c = (abs r :+ abs i) ** 2 + c

{-# INLINE julia #-}
julia :: Comp -> Fractal
julia p _ z = z * z + p

{-# INLINE countIterations #-}
countIterations :: Double -> Int -> Fractal -> Comp -> Int
countIterations maxAbs maxIter fractal p = go 1 p
  where
    go !i !z | i >= maxIter || magSquared z >= maxAbs = i
             | otherwise                              = go (i + 1) (fractal p z)
