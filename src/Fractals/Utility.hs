{-# LANGUAGE BangPatterns #-}
module Fractals.Utility where

{-# INLINE (.:) #-}
(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) f g a b = f (g a b)

{-# INLINE square #-}
square :: Num a => a -> a
square x = x * x

{-# INLINE grid #-}
grid :: (Ord a, Num a, Num b) => (a, a) -> (b, b) -> (b, b) -> (b -> b -> c) -> [[c]]
grid (!w, !h) (!x1, !y1) (!dx, !dy) f = goi 0 y1
  where
    goi !i !y | i < h     = goj 0 x1 : goi (i + 1) (y + dy)
              | otherwise = []
      where
        goj !j !x | j < w     = f x y : goj (j + 1) (x + dx)
                  | otherwise = []
