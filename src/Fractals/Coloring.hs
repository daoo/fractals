{-# LANGUAGE MagicHash #-}
module Fractals.Coloring
  ( ascii
  , greyscale
  ) where

import Data.Word
import GHC.Exts

{-# INLINE unsafeQuot #-}
unsafeQuot :: Int -> Int -> Int
unsafeQuot (I# x) (I# y) = I# (quotInt# x y)

{-# INLINE scale #-}
scale :: Int -> Int -> Int -> Int
scale t m i = i * t `unsafeQuot` (m + 1)

{-# INLINE ascii #-}
ascii :: Int -> Int -> Char
ascii m i = chars !! scale (length chars) m i
  where
    chars = " -~+*=#%@&$"

{-# INLINE greyscale #-}
greyscale :: Int -> Int -> Word8
greyscale m i = fromIntegral $ scale 255 m i
