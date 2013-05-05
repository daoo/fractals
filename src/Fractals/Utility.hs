{-# LANGUAGE BangPatterns #-}
module Fractals.Utility where

{-# INLINE square #-}
square :: Num a => a -> a
square x = x * x

{-# INLINE grid #-}
grid :: (Int, Int) -> (Int -> Int -> a) -> [[a]]
grid (w, h) f = goy 0
  where
    goy !y | y < h     = gox 0 : goy (y + 1)
           | otherwise = []
      where
        gox !x | x < w     = f x y : gox (x + 1)
               | otherwise = []

{-# INLINE showGrid #-}
-- |Apply a show function to a grid.
-- Note that showGrid id = unlines.
showGrid :: (a -> Char) -> [[a]] -> String
showGrid f = go
  where
    go []          = []
    go [[]]        = []
    go ([]:xs)     = '\n' : go xs
    go ((y:ys):xs) = f y : go (ys:xs)
