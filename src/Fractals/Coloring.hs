{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Fractals.Coloring
  ( Greyscale
  , RGB
  , Color(..)
  , ascii
  , greyscale
  ) where

import Data.Word
import Fractals.Utility

type Greyscale = Word8
type RGB       = (Word8, Word8, Word8)

class Color c where
  toRgb :: c -> RGB

instance Color Greyscale where
  {-# INLINE toRgb #-}
  toRgb c = (c, c, c)

instance Color RGB where
  {-# INLINE toRgb #-}
  toRgb = id

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
