module Fractals.Coloring
  ( RGB
  , ascii
  , greyscale
  ) where

import Data.Word
import Fractals.Utility

type RGB = (Word8, Word8, Word8)

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
greyscale :: Int -> Int -> RGB
greyscale m i = (c, c, c) where c = fromIntegral $ scale m 255 i
