module Fractals.Data.Size where

import Data.Word

-- |Definition of the size of a rectangle in 2D space.
data Size = Size { width :: !Int, height :: !Int }
  deriving Show

{-# INLINE mkSize #-}
-- |Construct a 'Size'.
--
-- Using any 'Word' that don't fit in a 'Int' is undefined.
mkSize :: Word -> Word -> Size
mkSize w h = Size (fromIntegral w) (fromIntegral h)

{-# INLINE getArea #-}
-- |Calculate the area of a 'Size'.
getArea :: Size -> Int
getArea (Size w h) = w * h
