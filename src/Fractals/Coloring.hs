module Fractals.Coloring
  ( Greyscale
  , RGB
  , RGBA
  , greyscaleToRgba
  , ascii
  , greyscale
  ) where

import Data.Word
import Fractals.Utility

type Greyscale = Word8
type RGB       = (Word8, Word8, Word8)
type RGBA      = (Word8, Word8, Word8, Word8)

{-# INLINE greyscaleToRgba #-}
greyscaleToRgba :: Greyscale -> RGBA
greyscaleToRgba c = (c, c, c, 255)

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
greyscale :: Int -> Int -> Greyscale
greyscale m i = fromIntegral $ scale m 255 i
