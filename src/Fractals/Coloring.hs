{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Fractals.Coloring
  ( ascii
  , greyscale
  ) where

import Codec.Picture.Types
import Fractals.Definitions (Iterations)
import Fractals.Utility

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
          -> Pixel8
greyscale m i = fromIntegral $ scale m 255 i
