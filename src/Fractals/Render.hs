{-# LANGUAGE BangPatterns #-}
module Fractals.Render
  ( rgbaArray
  , fillRgbaArray
  , lists
  , string
  ) where

import Data.Array.Base (unsafeWrite)
import Data.Array.IO hiding (unsafeFreeze)
import Data.Word
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

type RgbaArray = IOUArray (Int, Int, Int) Word8

{-# INLINE newRgbaArray #-}
newRgbaArray :: Area -> IO RgbaArray
newRgbaArray area = newArray_ ((0,0,0), (h-1,w-1,3))
  where (w, h) = areaScreen area

{-# INLINE rgbaArray #-}
rgbaArray :: Definition -> Int -> R -> Area -> IO (IOUArray (Int, Int, Int) Word8)
rgbaArray !fractal !iter !maxabs !area = newRgbaArray area >>= fillRgbaArray
  (areaScreen area)
  (areaTopLeft area)
  (areaDelta area)
  (\x y -> greyscale iter $ fractal (x:+y) maxabs iter)

{-# INLINE fillRgbaArray #-}
fillRgbaArray :: (Int, Int) -> Comp -> Comp -> (R -> R -> RGB) -> RgbaArray -> IO RgbaArray
fillRgbaArray (!w, !h) (x1:+y1) (dx:+dy) f arr = go 0 0 x1 y1
  where
    n = 4 * w * h

    write i (r, g, b) = do
      unsafeWrite arr i r
      unsafeWrite arr (i + 1) g
      unsafeWrite arr (i + 2) b
      unsafeWrite arr (i + 3) 255

    go !i !j !x !y
      | i == w    = go 0 j x1 (y+dy)
      | j == n    = return arr
      | otherwise = write j (f x y) >> go (i+1) (j+4) (x+dx) y
