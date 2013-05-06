{-# LANGUAGE BangPatterns #-}
module Fractals.Show
  ( showFractal
  ) where

import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Utility

{-# INLINE showFractal #-}
showFractal :: Definition -> Int -> R -> Area -> String
showFractal fractal iter maxabs (Area topleft _ delta screen) =
  grid screen topleft delta (ascii iter .: func)
  where
    func x y = fractal (x :+ y) maxabs iter

{-# INLINE grid #-}
grid :: (Int, Int) -> (R, R) -> (R, R) -> (R -> R -> Char) -> String
grid (!w, !h) (!x1, !y1) (!dx, !dy) f = go 0 0 x1 y1
  where
    go !i !j !x !y | i == w    = '\n' : go 0 (j+1) x1 (y + dy)
                   | j == h    = []
                   | otherwise = f x y : go (i+1) j (x + dx) y
