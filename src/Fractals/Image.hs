{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Fractals.Image where

import Data.Array.Base (unsafeWrite)
import Data.Array.IO
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions

{-# INLINE newRgbPtr #-}
newRgbPtr :: (Int, Int) -> IO (Ptr Word8)
newRgbPtr (w, h) = mallocArray $ w * h * 3

freeRgbPtr :: Ptr Word8 -> IO ()
freeRgbPtr = free

newRgbaArray :: (MArray a Word8 m) => (Int, Int) -> m (a (Int, Int, Int) Word8)
newRgbaArray (w, h) = newArray_ ((0,0,0), (h-1,w-1,3))

{-# INLINE writeRgbPtr #-}
writeRgbPtr :: Color c => Ptr Word8 -> Int -> c -> IO ()
writeRgbPtr ptr n c = do
  let (r, g, b) = toRgb c
      ptr' = plusPtr ptr n
  poke ptr' r
  poke (plusPtr ptr' 1) g
  poke (plusPtr ptr' 2) b

{-# INLINE writeRgbaArray #-}
writeRgbaArray :: (Color c, MArray a Word8 m) => a (Int, Int, Int) Word8 -> Int -> c -> m ()
writeRgbaArray t n c = do
  let (r, g, b) = toRgb c
  unsafeWrite t n r
  unsafeWrite t (n+1) g
  unsafeWrite t (n+2) b
  unsafeWrite t (n+3) (255 :: Word8)

{-# INLINE fillRgbaArray #-}
fillRgbaArray :: (Color c, MArray a Word8 m) =>
  (Int -> Int -> c)
  -> Definition
  -> Int
  -> R
  -> Area
  -> a (Int, Int, Int) Word8
  -> m ()
fillRgbaArray color fractal iter maxabs area arr =
  filler
    4
    (areaScreen area)
    (areaTopLeft area)
    (areaDelta area)
    (\n x y -> writeRgbaArray arr n $ color iter $ fractal (x:+y) maxabs iter)

{-# INLINE filler #-}
filler :: Monad m
  => Int
  -> (Int, Int)
  -> Comp
  -> Comp
  -> (Int -> R -> R -> m ())
  -> m ()
filler d (w, h) (x1:+y1) (dx:+dy) f = go 0 0 x1 y1
  where
    n = d * w * h

    go !i !j !x !y
      | i == w    = go 0 j x1 (y+dy)
      | j == n    = return ()
      | otherwise = f j x y >> go (i+1) (j+d) (x+dx) y
