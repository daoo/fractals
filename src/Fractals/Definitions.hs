{-# LANGUAGE BangPatterns, MagicHash #-}
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

import Data.Complex
import GHC.Base

type Definition = (Double, Int) -> Complex Double -> Int

{-# INLINE mandelbrot #-}
mandelbrot :: Int -> Definition
mandelbrot !a !t !p = iterations t (0:+0) (\z -> z ^ a + p)

{-# INLINE mandelbrot2 #-}
mandelbrot2 :: Definition
mandelbrot2 !t !p = go 0 (0:+0)
  where
    go :: Int -> Complex Double -> Int
    go !i (a:+b) = if check t (abs2, i) then i else go (i+1) (z2+p)
      where
        a2 = a*a
        b2 = b*b
        z2 = (a2-b2) :+ (2*a*b)
        abs2 = a2+b2

{-# INLINE mandelbrot3 #-}
mandelbrot3 :: Definition
mandelbrot3 !t !p = iterations t (0:+0) (\z -> z * z * z + p)

{-# INLINE burningShip #-}
burningShip :: Definition
burningShip !t !p = iterations t (0:+0) (\(r:+i) -> square (abs r :+ abs i) + p)

{-# INLINE julia #-}
julia :: Complex Double -> Definition
julia !c !t !p = go 0 p
  where
    go :: Int -> Complex Double -> Int
    go !i (a:+b) = if check t (abs2, i) then i else go (i+1) (z2 + c)
      where
        a2 = a*a
        b2 = b*b
        z2 = (a2-b2) :+ (2*a*b)
        abs2 = a2+b2

{-# INLINE check #-}
check :: (Double, Int) -> (Double, Int) -> Bool
check (D# ma, I# mi) (D# a, I# i) = tagToEnum# ((a >=## ma) `orI#` (i >=# mi))

{-# INLINE iterations #-}
-- |Count the number of iterations in a point
iterations :: (Double, Int) -> Complex Double -> (Complex Double -> Complex Double) -> Int
iterations !t !z0 !f = go 0 z0
  where
    go !i !z = if check t (mag2 z, i) then i else go (i+1) (f z)

{-# INLINE mag2 #-}
mag2 :: Complex Double -> Double
mag2 (a:+b) = a*a + b*b

{-# INLINE square #-}
square :: Complex Double -> Complex Double
square z = z * z
