{-# LANGUAGE CPP #-}
module Fractals.Complex
  ( Comp(..)
  , realPart
  , imgPart
  , R
  , magnitudeSquared
  , (./)
  ) where

#ifdef RATIONAL
import Data.Ratio
type R = Rational
#else
type R = Double
#endif

infix 1 :+

-- |Definition of a complex number
data Comp = {-# UNPACK #-} !R :+ {-# UNPACK #-} !R
  deriving Show

{-# INLINE realPart #-}
realPart :: Comp -> R
realPart (r:+_) = r

{-# INLINE imgPart #-}
imgPart :: Comp -> R
imgPart (_:+i) = i

{-# INLINE magnitudeSquared #-}
-- |The magintude of a complex number squared.
-- Avoids the expensive square root.
magnitudeSquared :: Comp -> R
magnitudeSquared (a :+ b) = a*a + b*b

instance Num Comp where
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  (a:+b) + (c:+d) = (a + c) :+ (b + d)
  (a:+b) - (c:+d) = (a - c) :+ (b - d)
  (a:+b) * (c:+d) = (a*c - b*d) :+ (b*c + a*d)

  abs (a:+b) = abs a :+ abs b

  negate      = undefined
  signum      = undefined
  fromInteger = undefined

{-# INLINE (./) #-}
(./) :: Comp -> R -> Comp
(a:+b) ./ x = (a/x) :+ (b/x)
