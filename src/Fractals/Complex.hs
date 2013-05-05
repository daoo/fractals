module Fractals.Complex
  ( Comp(..)
  , R
  , magnitudeSquared
  ) where

type R = Double

data Comp = {-# UNPACK #-} !R :+ {-# UNPACK #-} !R
  deriving Show

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

  negate      = undefined
  abs         = undefined
  signum      = undefined
  fromInteger = undefined
