{-# LANGUAGE BangPatterns #-}
module Fractals.Definitions
  ( Definition
  , mandelbrot
  , burningShip
  , julia
  , mandelbrot2'
  ) where

import Fractals.Complex
import Fractals.Utility

type Definition = Comp -> R -> Int -> Int

{-# INLINE mandelbrot #-}
mandelbrot :: Int -> Definition
mandelbrot  1 !p = countIterations (0:+0) (+p)
mandelbrot  2 !p = countIterations (0:+0) (\z -> z * z + p)
mandelbrot  3 !p = countIterations (0:+0) (\z -> z * z * z + p)
mandelbrot !a !p = countIterations (0:+0) (\z -> z ^ a + p)

{-# INLINE burningShip #-}
burningShip :: Definition
burningShip !p = countIterations (0:+0) (\(r:+i) -> square (abs r :+ abs i) + p)

{-# INLINE julia #-}
julia :: Comp -> Definition
julia !c !p = countIterations p (\z -> z * z + c)

{-# INLINE countIterations #-}
-- |Count the number of iterations in a point
countIterations :: Comp -> (Comp -> Comp) -> R -> Int -> Int
countIterations !z0 znext !maxAbs !maxIter = go 0 z0
  where
    go !i !z = if i >= maxIter || magnitudeSquared z >= maxAbs
      then i
      else go (i + 1) (znext z)

{-# INLINE mandelbrot2' #-}
mandelbrot2' :: Definition
mandelbrot2' (px :+ py) !maxAbs !maxIter = go 0 (0, 0) (0, 0)
  where
    go !i (!x, !y) (!xx, !yy) = if i >= maxIter || (xx + yy) >= maxAbs
      then i
      else let xx' = x*x
               yy' = y*y
            in go (i + 1) (xx' - yy' + px, let a = x*y in a + a + py) (xx', yy')
