{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Fractals.Coloring
  ( Greyscale
  , RGB
  , RGBA
  , Color(..)
  , ascii
  , greyscale
  ) where

import Data.Word
import Fractals.Utility

type Greyscale = Word8
type RGB       = (Word8, Word8, Word8)
type RGBA      = (Word8, Word8, Word8, Word8)

class Color c where
  toRgb :: c -> RGB
  toRgba :: c -> RGBA

instance Color Greyscale where
  {-# INLINE toRgb #-}
  {-# INLINE toRgba #-}
  toRgb c  = (c, c, c)
  toRgba c = (c, c, c, 255)

instance Color RGB where
  {-# INLINE toRgb #-}
  {-# INLINE toRgba #-}
  toRgb            = id
  toRgba (r, g, b) = (r, g, b, 255)

instance Color RGBA where
  {-# INLINE toRgb #-}
  {-# INLINE toRgba #-}
  toRgb (r, g, b, _) = (r, g, b)
  toRgba             = id

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
