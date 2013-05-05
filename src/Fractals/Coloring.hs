module Fractals.Coloring
  ( ascii
  , greyscale
  ) where

import Data.Word

{-# INLINE scale #-}
scale :: Integral a => a -> a -> a -> a
scale t m i = i * t `div` (m + 1)

{-# INLINE ascii #-}
ascii :: Int -> Int -> Char
ascii m i = chars !! scale (length chars) m i
  where
    chars = " -~+*=#%@&$"

{-# INLINE greyscale #-}
greyscale :: Int -> Int -> Word8
greyscale m i = fromIntegral $ scale 255 m i
