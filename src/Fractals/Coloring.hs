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
scale t m i = (i * t) `unsafeQuot` (m + 1)

{-# INLINE ascii #-}
ascii :: Int -> Int -> Char
ascii m i = case scale 11 m i of
  1  -> '-'
  2  -> '~'
  3  -> '+'
  4  -> '*'
  5  -> '='
  6  -> '#'
  7  -> '%'
  8  -> '@'
  9  -> '&'
  10 -> '$'
  _  -> ' '

{-# INLINE greyscale #-}
greyscale :: Int -> Int -> (Word8, Word8, Word8)
greyscale m i = (c, c, c) where c = fromIntegral $ scale 256 m i
