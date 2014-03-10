{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Fractals.Coloring
  ( Greyscale(..)
  , RGB(..)
  , RGBA(..)
  , Color(..)
  , ascii
  , greyscale
  ) where

import Data.Word
import Fractals.Utility

newtype Greyscale = Greyscale { grey :: Word8 }

data RGB = RGB
  { rgbR :: {-# UNPACK #-} !Word8
  , rgbG :: {-# UNPACK #-} !Word8
  , rgbB :: {-# UNPACK #-} !Word8
  }

data RGBA = RGBA
  { rgbaR :: {-# UNPACK #-} !Word8
  , rgbaG :: {-# UNPACK #-} !Word8
  , rgbaB :: {-# UNPACK #-} !Word8
  , rgbaA :: {-# UNPACK #-} !Word8
  }

class Color c where
  toRgb  :: c -> RGB
  toRgba :: c -> RGBA

instance Color Greyscale where
  {-# INLINE toRgb #-}
  {-# INLINE toRgba #-}
  toRgb (Greyscale c)  = RGB c c c
  toRgba (Greyscale c) = RGBA c c c 255

instance Color RGB where
  {-# INLINE toRgb #-}
  {-# INLINE toRgba #-}
  toRgb              = id
  toRgba (RGB r g b) = RGBA r g b 255

instance Color RGBA where
  {-# INLINE toRgb #-}
  {-# INLINE toRgba #-}
  toRgb (RGBA r g b _) = RGB r g b
  toRgba               = id

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
greyscale m i = Greyscale $ fromIntegral $ scale m 255 i
