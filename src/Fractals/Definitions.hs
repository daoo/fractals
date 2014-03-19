{-# LANGUAGE BangPatterns #-}
module Fractals.Definitions
  ( Iterations
  , Definition
  , mandelbrot
  , mandelbrot2
  , mandelbrot3
  , burningShip
  , julia
  ) where

import Fractals.Complex
import Fractals.Utility

type Iterations = Int

type Definition = Comp -> R -> Iterations -> Iterations

{-# INLINE mandelbrot #-}
mandelbrot :: Int -> Definition
mandelbrot !a !p = countIterations (0:+0) (\z -> z ^ a + p)

{-# INLINE mandelbrot2 #-}
mandelbrot2 :: Definition
mandelbrot2 !p = countIterations (0:+0) (\z -> z * z + p)

{-# INLINE mandelbrot3 #-}
mandelbrot3 :: Definition
mandelbrot3 !p = countIterations (0:+0) (\z -> z * z * z + p)

{-# INLINE burningShip #-}
burningShip :: Definition
burningShip !p = countIterations (0:+0) (\(r:+i) -> square (abs r :+ abs i) + p)

{-# INLINE julia #-}
julia :: Comp -> Definition
julia !c !p = countIterations p (\z -> z * z + c)

{-# INLINE countIterations #-}
-- |Count the number of iterations in a point
countIterations :: Comp -> (Comp -> Comp) -> R -> Iterations -> Iterations
countIterations !z0 znext !maxAbs !maxIter = go 0 z0
  where
    go !i !z = if i >= maxIter || magnitudeSquared z >= maxAbs
      then i
      else go (i + 1) (znext z)
