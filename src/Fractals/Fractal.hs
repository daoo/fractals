module Fractals.Fractal
  ( Fractal(..)
  , mkFunction
  ) where

import Fractals.Complex
import Fractals.Definitions

data Fractal = Fractal
  { fractalDef     :: Definition
  , fractalIter    :: Int
  , fractalMaxAbs  :: R
  , fractalTopLeft :: (R, R)
  , fractalPlane   :: (R, R)
  , fractalScreen  :: (Int, Int)
  }

{-# INLINE mkFunction #-}
mkFunction :: Fractal -> Int -> Int -> Int
mkFunction (Fractal frac maxIter maxAbs (px, py) (pw, ph) (w, h)) =
  \x y -> frac (screenToPlane x y) maxAbs maxIter
  where
    dx =   pw / fromIntegral w
    dy = - ph / fromIntegral h

    real x = px + fromIntegral x * dx
    imag y = py + fromIntegral y * dy

    screenToPlane x y = real x :+ imag y
