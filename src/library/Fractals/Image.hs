{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
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
    write s (n+3) a

class Storage s c m where
  fill :: s -> (Int -> Int -> c) -> Definition -> Int -> R -> Area -> m ()

instance (Writable a Word8 m) => Storage a Word8 m where
  {-# INLINE fill #-}
  fill s = helper 1 (write s)

instance (Writable a Word8 m) => Storage a (Word8, Word8, Word8) m where
  {-# INLINE fill #-}
  fill s = helper 3 (write s)

instance (Writable a Word8 m) => Storage a (Word8, Word8, Word8, Word8) m where
  {-# INLINE fill #-}
  fill s = helper 4 (write s)

{-# INLINE newRgbaArray #-}
newRgbaArray :: (MArray a e m) => (Int, Int) -> m (a (Int, Int, Int) e)
newRgbaArray (w, h) = newArray_ ((0,0,0), (h-1,w-1, 3))

{-# INLINE newRgbPtr #-}
newRgbPtr :: (Int, Int) -> IO (Ptr Word8)
newRgbPtr (w, h) = mallocArray $ w * h * 3

{-# INLINE newRgbaPtr #-}
newRgbaPtr :: (Int, Int) -> IO (Ptr Word8)
newRgbaPtr (w, h) = mallocArray $ w * h * 4

{-# INLINE newGreyscalePtr #-}
newGreyscalePtr :: (Int, Int) -> IO (Ptr Word8)
newGreyscalePtr (w, h) = mallocArray $ w * h

{-# INLINE helper #-}
helper :: (Monad m, Color c)
  => Int
  -> (Int -> c -> m ())
  -> (Int -> Int -> c)
  -> Definition
  -> Int
  -> R
  -> Area
  -> m ()
helper n f color fractal iter maxabs area = monadic
  n
  (areaScreen area)
  (areaTopLeft area)
  (areaDelta area)
  (\i x y -> f i $ color iter $ fractal (x:+y) maxabs iter)