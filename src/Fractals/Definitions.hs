{-# LANGUAGE BangPatterns #-}
module Fractals.Definitions
  ( Definition
  , mandelbrot
  , burningShip
  , julia
  , countIterations
  ) where

import Fractals.Complex

type Definition = Comp -> Comp -> Comp

{-# INLINE square #-}
square :: Num a => a -> a
square x = x * x

{-# INLINE mandelbrot #-}
mandelbrot :: Int -> Definition
mandelbrot 0 c _ = c
mandelbrot 1 c z = z + c
mandelbrot 2 c z = z * z + c
mandelbrot a c z = z ^ a + c

{-# INLINE burningShip #-}
burningShip :: Definition
burningShip (r :+ i) c = square (abs r :+ abs i) + c

{-# INLINE julia #-}
julia :: Comp -> Definition
julia p _ z = z * z + p

{-# INLINE countIterations #-}
countIterations :: Double -> Int -> Definition -> Comp -> Int
countIterations maxAbs maxIter fractal p = go 1 p
  where
    go !i !z = if i >= maxIter || magSquared z >= maxAbs
      then i
      else go (i + 1) (fractal p z)
