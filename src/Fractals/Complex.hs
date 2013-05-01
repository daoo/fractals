module Fractals.Complex
  ( Comp(..)
  , magnitude
  , magnitudeSquared
  ) where

type R = Double

data Comp = {-# UNPACK #-} !R :+ {-# UNPACK #-} !R

magnitude :: Comp -> R
magnitude (a:+b) = sqrt (a*a + b*b)

{-# INLINE magnitudeSquared #-}
magnitudeSquared :: Comp -> R
magnitudeSquared (a :+ b) = a*a + b*b

instance Num Comp where
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  (a:+b) + (c:+d) = (a + c) :+ (b + d)
  (a:+b) - (c:+d) = (a - c) :+ (b - d)
  (a:+b) * (c:+d) = (a*c - b*d) :+ (b*c + a*d)

  {-# INLINE negate #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  {-# INLINE fromInteger #-}
  negate (a:+b)   = negate a :+ negate b
  abs z           = magnitude z :+ 0
  signum (0:+0)   = 0
  signum z@(a:+b) = (a/r) :+ (b/r) where r = magnitude z
  fromInteger n   = fromInteger n :+ 0
