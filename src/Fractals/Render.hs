{-# LANGUAGE BangPatterns #-}
module Fractals.Render
  ( loop
  ) where

import Fractals.Complex
import Fractals.Geometry

{-# INLINE loop #-}
loop :: Monad m
  => Int
  -> Size
  -> Comp
  -> Comp
  -> (Int -> R -> R -> m ())
  -> m ()
loop d (Vec w h) (x1:+y1) (dx:+dy) f = go 0 0 x1 y1
  where
    n = d * w * h

    go !i !j !x !y
      | i == w    = go 0 j x1 (y+dy)
      | j == n    = return ()
      | otherwise = f j x y >> go (i+1) (j+d) (x+dx) y
