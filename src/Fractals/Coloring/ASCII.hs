module Fractals.Coloring.ASCII where

import Data.Char
import Data.Word
import Fractals.Math

{-# INLINE ascii #-}
-- |Find a ASCII character representation of a number of iterations.
ascii :: Int -- ^Maximum number of iterations
      -> Int -- ^Number of iterations for some pixel
      -> Char
ascii m i = case unsafeScaleInt m 10 i of
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
  _  -> 'E'

{-# INLINE asciiWord8 #-}
asciiWord8 :: Int -> Int -> Word8
asciiWord8 m i = fromIntegral $ ord $ ascii m i
