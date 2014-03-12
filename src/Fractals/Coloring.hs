{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Fractals.Coloring
  ( Greyscale(..)
  , RGB(..)
  , RGBA(..)
  , greyscaleToRGBA
  , ascii
  , greyscale
  ) where

import Data.Word
import Fractals.Definitions (Iterations)
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

greyscaleToRGBA :: Greyscale -> RGBA
greyscaleToRGBA (Greyscale c) = RGBA c c c 255

{-# INLINE ascii #-}
-- |Find a ASCII character representation of a number of iterations.
ascii :: Iterations -- ^Maximum number of iterations
      -> Iterations -- ^Number of iterations for some pixel 
      -> Char
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
-- |Find a single channel 8-bit color representation of a number of iterations.
greyscale :: Iterations -- ^Maximum number of iterations
          -> Iterations -- ^Number of iterations for som pixel
          -> Greyscale
greyscale m i = Greyscale $ fromIntegral $ scale m 255 i
