{-# LANGUAGE TypeOperators #-}
module Fractals.Output
  ( showFractal
  ) where

import Data.Array.Repa (Array(..), D, Z(..), DIM2, (:.)(..))
import qualified Data.Array.Repa as R

{-# INLINE showASCII #-}
showASCII :: Int -> Int -> Char
showASCII m i = chars !! ((i * length chars) `div` (m + 1))
  where
    chars = " -~+*=#%@&$"

showFractal :: Int -> Array D DIM2 Int -> String
showFractal iters array = R.toList $ R.computeUnboxedS $ R.traverse array e f
  where
    (Z :. _ :. width) = R.extent array

    e (Z :. h :. w) = Z :. h :. (w + 1)

    f g i@(Z :. _ :. x) | x < width = showASCII iters (g i)
                        | otherwise = '\n'
