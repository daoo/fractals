{-# LANGUAGE TypeOperators #-}
module Fractals.Image
  ( writeFractal
  ) where

import Data.Array.Repa (Array(..), D, DIM2, Z(..), (:.)(..))
import Data.Array.Repa.IO.DevIL
import Data.Word
import Fractals.Coloring
import Fractals.Complex
import Fractals.Fractal
import qualified Data.Array.Repa as R

write :: FilePath -> Array D DIM2 Word8 -> IO ()
write fp = runIL . writeImage fp . Grey . R.computeS

writeFractal :: Fractal -> FilePath -> IO ()
writeFractal fractal fp = write fp $ R.fromFunction (Z :. h :. w) $
  \(Z :. y :. x) -> greyscale (fractalIter fractal) $ func x y
  where
    func x y = fractalDef fractal (screenToPlane x y) (fractalMaxAbs fractal) (fractalIter fractal)

    (w, h)   = fractalScreen fractal
    (px, py) = fractalTopLeft fractal
    (pw, ph) = fractalPlane fractal

    dx =   pw / fromIntegral w
    dy = - ph / fromIntegral h

    real x = px + fromIntegral x * dx
    imag y = py + fromIntegral y * dy

    screenToPlane x y = real x :+ imag y
