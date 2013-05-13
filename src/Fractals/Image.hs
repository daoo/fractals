{-# LANGUAGE BangPatterns, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}
module Fractals.Image where

import Control.Applicative
import Data.Array.Base (unsafeWrite)
import Data.Array.IO
import Data.Word
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions

newtype Image c a i e = Image { mkArray :: a i e }

type RgbaImage = Image RGBA IOUArray (Int, Int, Int) Word8

class (Monad m, MArray a e m, Color c, Ix i) => ImageArray c a i e m where
  new :: (Int, Int) -> m (Image c a i e)
  write :: Image c a i e -> Int -> c -> m ()
  create :: MArray a e m => (Int -> Int -> c) -> Definition -> Int -> R -> Area -> m (Image c a i e)

instance ImageArray Greyscale IOUArray (Int, Int) Word8 IO where
  {-# INLINE new #-}
  new (w, h) = Image <$> newArray_ ((0,0), (h-1,w-1))

  {-# INLINE write #-}
  write (Image arr) n c = do
    unsafeWrite arr n c

  {-# INLINE create #-}
  create !color !fractal !iter !maxabs !area = do
    i <- new (areaScreen area)
    fillArray
      (areaScreen area)
      (areaTopLeft area)
      (areaDelta area)
      (\n x y -> write i n $ color iter $ fractal (x:+y) maxabs iter)
      (+1)
    return i

instance ImageArray RGBA IOUArray (Int, Int, Int) Word8 IO where
  {-# INLINE new #-}
  new (w, h) = Image <$> newArray_ ((0,0,0), (h-1,w-1,3))

  {-# INLINE write #-}
  write (Image arr) n (r, g, b, a) = do
    unsafeWrite arr n r
    unsafeWrite arr (n+1) g
    unsafeWrite arr (n+2) b
    unsafeWrite arr (n+3) a

  {-# INLINE create #-}
  create !color !fractal !iter !maxabs !area = do
    i <- new (areaScreen area)
    fillArray
      (areaScreen area)
      (areaTopLeft area)
      (areaDelta area)
      (\n x y -> write i n $ color iter $ fractal (x:+y) maxabs iter)
      (+4)
    return i

{-# INLINE fillArray #-}
fillArray :: (Monad m)
  => (Int, Int)
  -> Comp
  -> Comp
  -> (Int -> R -> R -> m ())
  -> (Int -> Int)
  -> m ()
fillArray (!w, !h) (x1:+y1) (dx:+dy) f next = go 0 0 x1 y1
  where
    n = 4 * w * h

    go !i !j !x !y
      | i == w    = go 0 j x1 (y+dy)
      | j == n    = return ()
      | otherwise = f j x y >> go (i+1) (next j) (x+dx) y
