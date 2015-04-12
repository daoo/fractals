{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
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
  , boxDefinition
  , unboxComplex
  ) where

import Data.Complex
import GHC.Base

type Complex# = (# Double#, Double# #)

unboxComplex :: Complex Double -> Complex#
unboxComplex (D# r :+ D# i) = (# r, i #)

(.+) :: Complex# -> Complex# -> Complex#
(# ar, ai #) .+ (# br, bi #) = (# ar +## br, ai +## bi #)

(.*) :: Complex# -> Complex# -> Complex#
(# ar, ai #) .* (# br, bi #) =
  (# ar *## br -## ai *## bi, ar *## bi +## ai *## br #)

(.^) :: Complex# -> Int# -> Complex#
(# r, i #) .^ x = unboxComplex ((D# r :+ D# i) ^ I# x)

square :: Complex# -> Complex#
square (# a, b #) = z2
  where
    a2 = a *## a
    b2 = b *## b
    ab = a *## b
    z2 = (# a2 -## b2, ab +## ab #)

squareAbs :: Complex# -> (# Complex#, Double# #)
squareAbs (# a, b #) = (# z2, abs2 #)
  where
    a2 = a *## a
    b2 = b *## b
    ab = a *## b
    z2 = (# a2 -## b2, ab +## ab #)
    abs2 = a2 +## b2

mag2 :: Complex# -> Double#
mag2 (# a, b #) = (a *## a) +## (b *## b)

cabs :: Complex# -> Complex#
cabs (# a, b #) = (# a', b' #)
  where
    !(D# a') = abs (D# a)
    !(D# b') = abs (D# b)

type Definition = (# Double#, Int# #) -> Complex# -> Int#

{-# INLINE boxDefinition #-}
boxDefinition :: Definition -> (Double, Int) -> Complex Double -> Int
boxDefinition f (D# maxabs, I# maxiter) (D# r :+ D# i) = I# (f (# maxabs, maxiter #) (# r, i #))

{-# INLINE mandelbrot #-}
mandelbrot :: Int# -> Definition
mandelbrot a t p = iterations t (# 0.0##, 0.0## #) $
  \z -> (# (z .^ a) .+ p, mag2 z #)

{-# INLINE mandelbrot2 #-}
mandelbrot2 :: Definition
mandelbrot2 t p = iterations t (# 0.0##, 0.0## #) $
  \z -> let (# z2, abs2 #) = squareAbs z in (# z2 .+ p, abs2 #)

{-# INLINE mandelbrot3 #-}
mandelbrot3 :: Definition
mandelbrot3 t p = iterations t (# 0.0##, 0.0## #) $
  \z -> (# (z .* z .* z) .+ p, mag2 z #)

{-# INLINE burningShip #-}
burningShip :: Definition
burningShip t p = iterations t (# 0.0##, 0.0## #) $
  \z -> (# square (cabs z) .+ p, mag2 z #)

{-# INLINE julia #-}
julia :: Complex# -> Definition
julia c t p = iterations t p $
  \z -> let (# z2, abs2 #) = squareAbs z in (# z2 .+ c, abs2 #)

{-# INLINE check #-}
check :: (# Double#, Int# #) -> Double# -> Int# -> Bool
check (# ma, mi #) a i = tagToEnum# (i >=# mi) || tagToEnum# (a >=## ma)

{-# INLINE iterations #-}
-- |Count the number of iterations in a point
iterations :: (# Double#, Int# #) -> Complex# -> (Complex# -> (# Complex#, Double# #)) -> Int#
iterations t z0 f = go 0# z0
  where
    go :: Int# -> Complex# -> Int#
    go i z = if check t abs2 i then i else go (i +# 1#) z'
      where
        (# z', abs2 #) = f z
