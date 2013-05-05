{-# LANGUAGE BangPatterns #-}
module Fractals.Utility where

{-# INLINE (.:) #-}
(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) f g a b = f (g a b)

{-# INLINE square #-}
square :: Num a => a -> a
square x = x * x

{-# INLINE grid #-}
grid :: (Int, Int) -> (Int -> Int -> a) -> [[a]]
grid (!w, !h) f = goy 0
  where
    goy !y | y < h     = gox 0 : goy (y + 1)
           | otherwise = []
      where
        gox !x | x < w     = f x y : gox (x + 1)
               | otherwise = []
