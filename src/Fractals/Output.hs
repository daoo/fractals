{-# LANGUAGE TypeOperators #-}
module Fractals.Output
  ( showFractal
  , writeFractal
  ) where

import Data.Array.Repa (Array(..), D, Z(..), DIM2, (:.)(..))
import Data.Array.Repa.IO.DevIL
import Data.Word
import qualified Data.Array.Repa as R

{-# INLINE scale #-}
scale :: Integral a => a -> a -> a -> a
scale t m i = i * t `div` (m + 1)

{-# INLINE greyscale #-}
greyscale :: Int -> Int -> Word8
greyscale m i = fromIntegral $ scale 255 m i

{-# INLINE showASCII #-}
showASCII :: Int -> Int -> Char
showASCII m i = chars !! scale (length chars) m i
  where
    chars = " -~+*=#%@&$"

showFractal :: Int -> Array D DIM2 Int -> String
showFractal iters array =
  R.toList $ R.computeUnboxedS $ R.traverse array e f
  where
    (Z :. _ :. width) = R.extent array

    e (Z :. h :. w) = Z :. h :. (w + 1)

    f g i@(Z :. _ :. x) | x < width = showASCII iters (g i)
                        | otherwise = '\n'

writeFractal :: Int -> FilePath -> Array D DIM2 Int -> IO ()
writeFractal iters fp array =
  runIL $ writeImage fp $ Grey $ R.computeS $ R.map (greyscale iters) array
