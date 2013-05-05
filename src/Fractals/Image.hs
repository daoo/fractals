{-# LANGUAGE TypeOperators #-}
module Fractals.Image
  ( writeFractal
  ) where

import Data.Array.Repa (Array(..), D, DIM2, Z(..), (:.)(..))
import Data.Array.Repa.IO.DevIL
import Data.Word
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import qualified Data.Array.Repa as R

{-# INLINE write #-}
write :: FilePath -> Array D DIM2 Word8 -> IO ()
write fp = runIL . writeImage fp . Grey . R.computeS

{-# INLINE writeFractal #-}
writeFractal :: Definition -> Int -> R -> Area -> FilePath -> IO ()
writeFractal fractal iter maxabs area fp = write fp $ R.fromFunction (Z :. h :. w) $
  \(Z :. y :. x) -> greyscale iter $ func x y
  where
    func x y = fractal (screenToPlane x y) maxabs iter

    (w, h)   = areaScreen area
    (px, py) = areaTopLeft area
    (pw, ph) = areaPlane area

    dx =   pw / fromIntegral w
    dy = - ph / fromIntegral h

    real x = px + fromIntegral x * dx
    imag y = py + fromIntegral y * dy

    screenToPlane x y = real x :+ imag y
