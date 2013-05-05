module Fractals.Fractal
  ( Fractal(..)
  , render
  ) where

import Fractals.Complex
import Fractals.Definitions
import qualified Data.Array.Repa as R

data Fractal = Fractal
  { fractalDef     :: Definition
  , fractalIter    :: Int
  , fractalMaxAbs  :: R
  , fractalTopLeft :: (R, R)
  , fractalPlane   :: (R, R)
  , fractalScreen  :: (Int, Int)
  }

render :: Fractal -> R.Array R.D R.DIM2 Int
render (Fractal frac maxIter maxAbs (px, py) (pw, ph) (w, h)) =
  R.fromFunction size f
  where
    size = R.Z R.:. h R.:. w

    f (R.Z R.:. y R.:. x) = frac (screenToPlane x y) maxAbs maxIter

    dx =   pw / fromIntegral w
    dy = - ph / fromIntegral h

    real x = px + fromIntegral x * dx
    imag y = py + fromIntegral y * dy

    screenToPlane x y = real x :+ imag y
