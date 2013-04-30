module Fractals.Output
  ( showFractal
  ) where

import Data.List

{-# INLINE showASCII #-}
showASCII :: Int -> Int -> Char
showASCII m i = chars !! ((i * length chars) `div` (m + 1))
  where
    chars = " -~+*=#%@&$"

showFractal :: Int -> [[Int]] -> String
showFractal iters = intercalate "\n" . map (map $ showASCII iters)
