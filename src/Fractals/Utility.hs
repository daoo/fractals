{-# LANGUAGE BangPatterns #-}
module Fractals.Utility where

{-# INLINE grid #-}
grid :: (Int, Int) -> (Int -> Int -> a) -> [[a]]
grid (w, h) f = goy 0
  where
    goy !y | y < h     = gox 0 : goy (y + 1)
           | otherwise = []
      where
        gox !x | x < w     = f x y : gox (x + 1)
               | otherwise = []
