{-# LANGUAGE BangPatterns #-}
-- |Definitions of some fractals.
--
-- Note that these have specialized hand-optimized implementations rather than
-- the more general and simpler code. This is for performance reasons only,
-- given a sufficently smart compiler the optimal output should be reached
-- anyway.
module Fractals.Definitions
  ( Definition
  , mandelbrot
  , mandelbrot2
  , mandelbrot3
  , burningShip
  , julia
  ) where

import Fractals.Complex
import Fractals.Math (square)

type Definition = (R, Int) -> Complex R -> Int

mandelbrot :: Int -> Definition
mandelbrot !a !t !p = iterations t (0:+0) (\z -> z ^ a + p)

mandelbrot2 :: Definition
mandelbrot2 !t !p = go 0 (0:+0)
  where
    go :: Int -> Complex R -> Int
    go !i (a:+b) = if check t (abs2, i) then i else go (i+1) (z2+p)
      where
        a2 = a*a
        b2 = b*b
        z2 = (a2-b2) :+ (2*a*b)
        abs2 = a2+b2

mandelbrot3 :: Definition
mandelbrot3 !t !p = iterations t (0:+0) (\z -> z * z * z + p)

burningShip :: Definition
burningShip !t !p = iterations t (0:+0) (\(r:+i) -> square (abs r :+ abs i) + p)

julia :: Complex R -> Definition
julia !c !t !p = iterations t p (\z -> z * z + c)

{-# INLINE check #-}
check :: (R, Int) -> (R, Int) -> Bool
check (!ma, !mi) (!a, !i) = a >= ma || i >= mi

{-# INLINE iterations #-}
-- |Count the number of iterations in a point
iterations :: (R, Int) -> Complex R -> (Complex R -> Complex R) -> Int
iterations (!maxAbs, !maxIter) !z0 !f = go 0 z0
  where
    go !i !z
      | i >= maxIter || magnitudeSquared z >= maxAbs = i
      | otherwise                                    = go (i + 1) (f z)
