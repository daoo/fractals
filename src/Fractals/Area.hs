module Fractals.Area
  ( Area(..)
  , rectangles
  , centered
  ) where

import Fractals.Complex

data Area = Area
  { imageWidth    :: {-# UNPACK #-} !Int
  , imageHeight   :: {-# UNPACK #-} !Int
  , complexWidth  :: {-# UNPACK #-} !R
  , complexHeight :: {-# UNPACK #-} !R
  , complexX      :: {-# UNPACK #-} !R
  , complexY      :: {-# UNPACK #-} !R
  , complexDX     :: {-# UNPACK #-} !R
  , complexDY     :: {-# UNPACK #-} !R
  }

rectangles :: (Int, Int) -> (R, R) -> (R, R) -> Area
rectangles (sw, sh) (cw, ch) (x, y) = Area sw sh cw ch x y dx dy
  where
    dx = cw / fromIntegral sw
    dy = -ch / fromIntegral sh

centered :: (Int, Int) -> (R, R) -> Area
centered (sw, sh) (cw, ch) = Area sw sh cw ch x y dx dy
  where
    x = -cw / 2
    y = ch / 2

    dx = cw / fromIntegral sw
    dy = -ch / fromIntegral sh
