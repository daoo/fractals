{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Fractals.Image
  ( writeFractal
  ) where

import Codec.Image.DevIL
import Data.Array.Base (unsafeWrite)
import Data.Array.ST
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions

{-# INLINE writeFractal #-}
writeFractal :: Definition -> Int -> R -> Area -> FilePath -> IO ()
writeFractal !fractal !iter !maxabs !area !fp = do
  ilInit
  writeImage fp $ runSTUArray $ build (areaScreen area) (areaTopLeft area) (areaDelta area) func
  where
    func !x !y = greyscale iter $ fractal (x:+y) maxabs iter

type Index = (Int, Int, Int)

{-# INLINE build #-}
build :: (MArray a Word8 m) => (Int, Int) -> (R, R) -> (R, R) -> (R -> R -> (Word8, Word8, Word8)) -> m (a Index Word8)
build (!w, !h) (!x1, !y1) (!dx, !dy) f = newArray_ ((0, 0, 0), (h-1, w-1, 3)) >>= go 0 0 x1 y1
  where
    n = 4 * w * h

    go !i !j !x !y !arr
      | i == w    = go 0 j x1 (y+dy) arr
      | j == n    = return arr
      | otherwise = do
        let (r, g, b) = f x y
        unsafeWrite arr (j + 0) r
        unsafeWrite arr (j + 1) g
        unsafeWrite arr (j + 2) b
        unsafeWrite arr (j + 3) 255
        go (i+1) (j+4) (x+dx) y arr
