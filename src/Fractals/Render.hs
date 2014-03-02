{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Fractals.Render
  ( string
  , loop
  ) where

import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Geometry

{-# INLINE string #-}
string :: Definition -> Int -> R -> Area -> String
string fractal iter maxabs area = buildString
  (areaScreen area)
  (areaTopLeft area)
  (areaDelta area)
  (\x y -> ascii iter $ fractal (x:+y) maxabs iter)

{-# INLINE buildString #-}
buildString :: Size -> Comp -> Comp -> (R -> R -> Char) -> String
buildString (Vec w h) (x1:+y1) (dx:+dy) f = go 0 0 x1 y1
  where
    go !i !j !x !y
      | i == w    = '\n' : go 0 (j+1) x1 (y+dy)
      | j == h    = []
      | otherwise = f x y : go (i+1) j (x+dx) y

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
