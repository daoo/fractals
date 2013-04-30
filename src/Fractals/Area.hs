module Fractals.Area
  ( Area(..)
  , rectangles
  , centered
  ) where

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

rectangles :: (Int, Int) -> (Double, Double) -> (Double, Double) -> Area
rectangles (sw, sh) (cw, ch) (x, y) = Area sw sh cw ch x y dx dy
  where
    dx = cw / fromIntegral sw
    dy = -ch / fromIntegral sh

centered :: (Int, Int) -> (Double, Double) -> Area
centered (sw, sh) (cw, ch) = Area sw sh cw ch x y dx dy
  where
    x = -cw / 2
    y = ch / 2

    dx = cw / fromIntegral sw
    dy = -ch / fromIntegral sh
