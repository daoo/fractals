module Fractals.Coloring
  ( ascii
  , greyscale
  ) where

import Data.Word
import Fractals.Utility

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
