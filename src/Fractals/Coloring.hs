module Fractals.Coloring
  ( ascii
  , greyscale
  ) where

import Data.Word
import Fractals.Utility

{-# INLINE ascii #-}
ascii :: Int -> Int -> Char
ascii m i = case scale m 10 i of
  0  -> ' '
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
  _  -> undefined

{-# INLINE greyscale #-}
greyscale :: Int -> Int -> (Word8, Word8, Word8)
greyscale m i = (c, c, c) where c = fromIntegral $ scale m 255 i
