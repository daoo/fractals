{-# LANGUAGE BangPatterns #-}
module Fractals.Utility where

{-# INLINE grid #-}
grid :: Num a => (Int, Int) -> (a, a) -> (a, a) -> (a -> a -> b) -> [[b]]
grid (w, h) (x1, y1) (dx, dy) f = goy y1 0
  where
    goy !a y | y < h     = gox x1 0 : goy (a + dy) (y + 1)
             | otherwise = []
      where
        gox !b x | x < w     = f b a : gox (b + dx) (x + 1)
                 | otherwise = []
