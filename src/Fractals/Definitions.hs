{-# LANGUAGE BangPatterns #-}
module Fractals.Definitions
  ( Iterations
  , Definition
  , mandelbrot
  , mandelbrot2
  , mandelbrot3
  , burningShip
  , julia
  , iterations
  ) where

import Fractals.Complex
import Fractals.Math (square)
import Numeric.FastMath ()

type Iterations = Int

type Definition = Complex R -> (Complex R, Complex R -> Complex R)

{-# INLINE mandelbrot #-}
mandelbrot :: Int -> Definition
mandelbrot !a !p = (0:+0, \z -> z ^ a + p)

{-# INLINE mandelbrot2 #-}
mandelbrot2 :: Definition
mandelbrot2 !p = (0:+0, \z -> z * z + p)

{-# INLINE mandelbrot3 #-}
mandelbrot3 :: Definition
mandelbrot3 !p = (0:+0, \z -> z * z * z + p)

{-# INLINE burningShip #-}
burningShip :: Definition
burningShip !p = (0:+0, \(r:+i) -> square (abs r :+ abs i) + p)

{-# INLINE julia #-}
julia :: Complex R -> Definition
julia !c !p = (p, \z -> z * z + c)

{-# INLINE iterations #-}
-- |Count the number of iterations in a point
iterations :: Definition -> R -> Iterations -> Complex R -> Iterations
iterations def !maxAbs !maxIter p = go 0 z0
  where
    (z0, znext) = def p

    go !i !z = if i >= maxIter || magnitudeSquared z >= maxAbs
      then i
      else go (i + 1) (znext z)
