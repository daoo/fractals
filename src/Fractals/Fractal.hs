module Fractals.Fractal
  ( Fractal(..)
  , newFractal
  ) where

import Fractals.Complex
import Fractals.Definitions

data Fractal = Fractal
  { fractalDef     :: Definition
  , fractalIter    :: Int
  , fractalMaxAbs  :: R
  , fractalTopLeft :: (R, R)
  , fractalPlane   :: (R, R)
  , fractalDelta   :: (R, R)
  , fractalScreen  :: (Int, Int)
  }

{-# INLINE newFractal #-}
newFractal :: Definition -> Int -> R -> (R, R) -> (R, R) -> (Int, Int) -> Fractal
newFractal def i a t plane@(pw, ph) screen@(w, h) =
  Fractal def i a t plane (dx, dy) screen
  where
    dx =   pw / fromIntegral w
    dy = - ph / fromIntegral h
