module Fractals.Complex
  ( module Data.Complex
  , magSquared
  , Comp
  , centered
  , Area(..)
  ) where

import Data.Complex

type Comp = Complex Double

{-# INLINE magSquared #-}
magSquared :: Num a => Complex a -> a
magSquared (r :+ i) = r * r + i * i

data Area = Area
  { imageWidth    :: {-# UNPACK #-} !Int
  , imageHeight   :: {-# UNPACK #-} !Int
  , complexWidth  :: {-# UNPACK #-} !Double
  , complexHeight :: {-# UNPACK #-} !Double
  , complexX      :: {-# UNPACK #-} !Double
  , complexY      :: {-# UNPACK #-} !Double
  , complexDX     :: {-# UNPACK #-} !Double
  , complexDY     :: {-# UNPACK #-} !Double
  }

centered :: (Int, Int) -> (Double, Double) -> Area
centered (sw, sh) (cw, ch) = Area sw sh cw ch x y dx dy
  where
    x = -cw / 2
    y = ch / 2

    dx = cw / fromIntegral sw
    dy = -ch / fromIntegral sh
