{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Fractals.Render
  ( rgbaArray
  , fillArray
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

{-# INLINE writeRGBA #-}
writeRGBA :: (Monad m, MArray a Word8 m, Ix i) => a i Word8 -> Int -> RGBA -> m ()
writeRGBA arr n (r, g, b, a) = do
  unsafeWrite arr n r
  unsafeWrite arr (n+1) g
  unsafeWrite arr (n+2) b
  unsafeWrite arr (n+3) a

{-# INLINE newRgbaArray #-}
newRgbaArray :: (Int, Int) -> IO (IOUArray (Int, Int, Int) Word8)
newRgbaArray (w, h) = newArray_ ((0,0,0), (h-1,w-1,3))

{-# INLINE rgbaArray #-}
rgbaArray :: (Color c)
  => (Int -> Int -> c)
  -> Definition
  -> Int
  -> R
  -> Area
  -> IO (IOUArray (Int, Int, Int) Word8)
rgbaArray !color !fractal !iter !maxabs !area =
  newRgbaArray (areaScreen area) >>= fillArray
    (areaScreen area)
    (areaTopLeft area)
    (areaDelta area)
    (\arr n x y -> writeRGBA arr n $ toRgba $ color iter $ fractal (x:+y) maxabs iter)
    (+4)

{-# INLINE fillRgbaArray #-}
fillRgbaArray :: (Color c)
  => (Int -> Int -> c)
  -> Definition
  -> Int
  -> R
  -> Area
  -> IOUArray (Int, Int, Int) Word8
  -> IO (IOUArray (Int, Int, Int) Word8)
fillRgbaArray color fractal iter maxabs area = fillArray
  (areaScreen area)
  (areaTopLeft area)
  (areaDelta area)
  (\arr n x y -> writeRGBA arr n $ toRgba $ color iter $ fractal (x:+y) maxabs iter)
  (+4)

{-# INLINE fillArray #-}
fillArray :: (Monad m, MArray a e m)
  => (Int, Int)
  -> Comp
  -> Comp
  -> (a i e -> Int -> R -> R -> m ())
  -> (Int -> Int)
  -> a i e
  -> m (a i e)
fillArray (!w, !h) (x1:+y1) (dx:+dy) f next arr = go 0 0 x1 y1
  where
    n = 4 * w * h

    go !i !j !x !y
      | i == w    = go 0 j x1 (y+dy)
      | j == n    = return arr
      | otherwise = f arr j x y >> go (i+1) (next j) (x+dx) y
