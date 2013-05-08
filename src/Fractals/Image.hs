{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Fractals.Image
  ( writeFractal
  ) where

import Codec.Image.DevIL
import Data.Array.Base (unsafeWrite)
import Data.Array.IO
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions

{-# INLINE writeFractal #-}
writeFractal :: Definition -> Int -> R -> Area -> FilePath -> IO ()
writeFractal !fractal !iter !maxabs !area !fp = ilInit >> build' >>= freeze >>= writeImage fp
  where
    build'   = build (areaScreen area) (areaTopLeft area) (areaDelta area) func
    func x y = greyscale iter $ fractal (x:+y) maxabs iter

{-# INLINE build #-}
build :: (Int, Int) -> (R, R) -> (R, R) -> (R -> R -> (Word8, Word8, Word8)) -> IO (IOUArray (Int, Int, Int) Word8)
build (!w, !h) (!x1, !y1) (!dx, !dy) f = newArray_ ((0,0,0), (h-1,w-1,3)) >>= go 0 0 x1 y1
  where
    n = 4 * w * h

    write arr i (r, g, b) = do
      unsafeWrite arr i r
      unsafeWrite arr (i + 1) g
      unsafeWrite arr (i + 2) b
      unsafeWrite arr (i + 3) 255

    go !i !j !x !y !arr
      | i == w    = go 0 j x1 (y+dy) arr
      | j == n    = return arr
      | otherwise = write arr j (f x y) >> go (i+1) (j+4) (x+dx) y arr
