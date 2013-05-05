module Fractals.Fractal
  ( Fractal(..)
  , render
  ) where

import Fractals.Complex
import Fractals.Definitions
import Fractals.Utility

data Fractal = Fractal
  { fractalDef     :: Definition
  , fractalIter    :: Int
  , fractalMaxAbs  :: R
  , fractalTopLeft :: (R, R)
  , fractalPlane   :: (R, R)
  , fractalScreen  :: (Int, Int)
  }

render :: Fractal -> [[Int]]
render (Fractal frac maxIter maxAbs (px, py) (pw, ph) screen@(w, h)) =
  grid screen (\x y -> frac (screenToPlane x y) maxAbs maxIter)
  where
    dx =   pw / fromIntegral w
    dy = - ph / fromIntegral h

    real x = px + fromIntegral x * dx
    imag y = py + fromIntegral y * dy

    screenToPlane :: Int -> Int -> Comp
    screenToPlane x y = real x :+ imag y
