{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Fractals.Image where

import Data.Array.Base (unsafeWrite)
import Data.Array.IO
import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Render

class Monad m => Writable s e m where
  write :: s -> Int -> e -> m ()

instance (MArray a e m, Ix i) => Writable (a i e) e m where
  {-# INLINE write #-}
  write = unsafeWrite

instance Storable e => Writable (Ptr e) e IO where
  {-# INLINE write #-}
  write ptr n = poke (plusPtr ptr n)

instance (Writable s e m) => Writable s (e, e, e) m where
  {-# INLINE write #-}
  write s n (r, g, b) = do
    write s n r
    write s (n+1) g
    write s (n+2) b

instance (Writable s e m) => Writable s (e, e, e, e) m where
  {-# INLINE write #-}
  write s n (r, g, b, a) = do
    write s n r
    write s (n+1) g
    write s (n+2) b
    write s (n+2) a

{-# INLINE newRgbaArray #-}
newRgbaArray :: (MArray a e m) => (Int, Int) -> m (a (Int, Int, Int) e)
newRgbaArray (w, h) = newArray_ ((0,0,0), (h-1,w-1, 3))

{-# INLINE newRgbaPtr #-}
newRgbaPtr :: (Int, Int) -> IO (Ptr Word8)
newRgbaPtr (w, h) = mallocArray $ w * h * 4

{-# INLINE fillRgbaArray #-}
fillRgbaArray :: (MArray a Word8 m, Color c)
  => (Int -> Int -> c)
  -> Definition
  -> Int
  -> R
  -> Area
  -> a (Int, Int, Int) Word8
  -> m ()
fillRgbaArray color fractal iter maxabs area s = monadic
  4
  (areaScreen area)
  (areaTopLeft area)
  (areaDelta area)
  (\n x y -> write s n $ toRgba $ color iter $ fractal (x:+y) maxabs iter)

{-# INLINE fillRgbaPtr #-}
fillRgbaPtr :: Color c
  => (Int -> Int -> c)
  -> Definition
  -> Int
  -> R
  -> Area
  -> Ptr Word8
  -> IO ()
fillRgbaPtr color fractal iter maxabs area ptr = monadic
  4
  (areaScreen area)
  (areaTopLeft area)
  (areaDelta area)
  (\n x y -> write ptr n $ toRgba $ color iter $ fractal (x:+y) maxabs iter)
