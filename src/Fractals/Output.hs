module Fractals.Output
  ( showASCII
  , showFractal
  ) where

import Fractals.Utility

{-# INLINE showASCII #-}
showASCII :: Int -> Int -> Char
showASCII m i = chars !! ((i * length chars) `div` (m + 1))
  where
    chars = " -~+*=#%@&$"

showFractal :: Int -> [[Int]] -> String
showFractal = showGrid . showASCII
