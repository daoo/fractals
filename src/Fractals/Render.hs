{-# LANGUAGE BangPatterns #-}
-- |Helper functions for rendering fractals.
module Fractals.Render
  ( loop
  ) where

import Fractals.Complex
import Fractals.Math

{-# INLINE loop #-}
-- |Render a square image using a monadic write function.
--
-- The write function takes an offset into the storage space and a complex
-- number.
loop :: Monad m
  => Int
  -> Size
  -> Complex R
  -> Complex R
  -> (Int -> Complex R -> m ())
  -> m ()
loop d size (x1:+y1) (dx:+dy) f = go 0 0 x1 y1
  where
    w = width size

    n = d * sizeArea size

    go !i !j !x !y
      | i == w    = go 0 j x1 (y+dy)
      | j == n    = return ()
      | otherwise = f j (x:+y) >> go (i+1) (j+d) (x+dx) y
