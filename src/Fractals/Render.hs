{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Fractals.Render
  ( lists
  , string
  ) where

import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions

{-# INLINE lists #-}
lists :: Definition -> Int -> R -> Area -> [[Int]]
lists fractal iter maxabs area = buildLists
  (areaScreen area)
  (areaTopLeft area)
  (areaDelta area)
  (\x y -> fractal (x:+y) maxabs iter)

{-# INLINE buildLists #-}
buildLists :: (Int, Int) -> Comp -> Comp -> (R -> R -> Int) -> [[Int]]
buildLists (!w, !h) (x1:+y1) (dx:+dy) f = goy 0 y1
  where
    goy !i !y | i < h     = gox 0 x1 : goy (i+1) (y+dy)
              | otherwise = []
      where
        gox !j !x | j < w     = f x y : gox (j+1) (x+dx)
                  | otherwise = []

{-# INLINE string #-}
string :: Definition -> Int -> R -> Area -> String
string fractal iter maxabs area = buildString
  (areaScreen area)
  (areaTopLeft area)
  (areaDelta area)
  (\x y -> ascii iter $ fractal (x:+y) maxabs iter)

{-# INLINE buildString #-}
buildString :: (Int, Int) -> Comp -> Comp -> (R -> R -> Char) -> String
buildString (!w, !h) (x1:+y1) (dx:+dy) f = go 0 0 x1 y1
  where
    go !i !j !x !y
      | i == w    = '\n' : go 0 (j+1) x1 (y+dy)
      | j == h    = []
      | otherwise = f x y : go (i+1) j (x+dx) y
