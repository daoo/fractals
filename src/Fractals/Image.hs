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

newtype ArrayImage a i c e = ArrayImage { mkArray :: a i e }
newtype PtrImage c e = PtrImage { mkPtr :: Ptr e }

freePtrImage :: PtrImage c e -> IO ()
freePtrImage = free . mkPtr

class (Monad m, Color c) => ImageArray c e i m where
  new   :: (Int, Int) -> m (i c e)
  write :: i c e -> Int -> c -> m ()
  fill  :: (Int -> Int -> c) -> Definition -> Int -> R -> Area -> i c e -> m ()

instance (Functor m, MArray a Word8 m) => ImageArray Greyscale Word8 (ArrayImage a (Int, Int)) m where
  {-# INLINE new #-}
  new (w, h) = ArrayImage `fmap` newArray_ ((0,0), (h-1,w-1))

  {-# INLINE write #-}
  write = unsafeWrite . mkArray

  {-# INLINE fill #-}
  fill color fractal iter maxabs area img =
    fillArray
      (areaScreen area)
      (areaTopLeft area)
      (areaDelta area)
      (\n x y -> write img n $ color iter $ fractal (x:+y) maxabs iter)
      1

instance (Functor m, MArray a Word8 m) => ImageArray RGB Word8 (ArrayImage a (Int, Int, Int)) m where
  {-# INLINE new #-}
  new (w, h) = ArrayImage `fmap` newArray_ ((0,0,0), (h-1,w-1,2))

  {-# INLINE write #-}
  write (ArrayImage arr) n (r, g, b) = do
    unsafeWrite arr n r
    unsafeWrite arr (n+1) g
    unsafeWrite arr (n+2) b

  {-# INLINE fill #-}
  fill color fractal iter maxabs area img =
    fillArray
      (areaScreen area)
      (areaTopLeft area)
      (areaDelta area)
      (\n x y -> write img n $ color iter $ fractal (x:+y) maxabs iter)
      3

instance (Functor m, MArray a Word8 m) => ImageArray RGBA Word8 (ArrayImage a (Int, Int, Int)) m where
  {-# INLINE new #-}
  new (w, h) = ArrayImage `fmap` newArray_ ((0,0,0), (h-1,w-1,3))

  {-# INLINE write #-}
  write (ArrayImage arr) n (r, g, b, a) = do
    unsafeWrite arr n r
    unsafeWrite arr (n+1) g
    unsafeWrite arr (n+2) b
    unsafeWrite arr (n+3) a

  {-# INLINE fill #-}
  fill color fractal iter maxabs area img =
    fillArray
      (areaScreen area)
      (areaTopLeft area)
      (areaDelta area)
      (\n x y -> write img n $ color iter $ fractal (x:+y) maxabs iter)
      4

instance ImageArray Greyscale Word8 PtrImage IO where
  {-# INLINE new #-}
  new (w, h) = PtrImage `fmap` mallocArray (w * h)

  {-# INLINE write #-}
  write (PtrImage ptr) n = poke (plusPtr ptr n)

  {-# INLINE fill #-}
  fill color fractal iter maxabs area img =
    fillArray
      (areaScreen area)
      (areaTopLeft area)
      (areaDelta area)
      (\n x y -> write img n $ color iter $ fractal (x:+y) maxabs iter)
      1

instance ImageArray RGB Word8 PtrImage IO where
  {-# INLINE new #-}
  new (w, h) = PtrImage `fmap` mallocArray (w * h * 3)

  {-# INLINE write #-}
  write (PtrImage ptr) n (r, g, b) = do
    let p = plusPtr ptr n
    poke p r
    poke (plusPtr p 1) g
    poke (plusPtr p 2) b

  {-# INLINE fill #-}
  fill color fractal iter maxabs area img =
    fillArray
      (areaScreen area)
      (areaTopLeft area)
      (areaDelta area)
      (\n x y -> write img n $ color iter $ fractal (x:+y) maxabs iter)
      3

instance ImageArray RGBA Word8 PtrImage IO where
  {-# INLINE new #-}
  new (w, h) = PtrImage `fmap` mallocArray (w * h * 4)

  {-# INLINE write #-}
  write (PtrImage ptr) n (r, g, b, a) = do
    let p = plusPtr ptr n
    poke p r
    poke (plusPtr p 1) g
    poke (plusPtr p 2) b
    poke (plusPtr p 3) a

  {-# INLINE fill #-}
  fill color fractal iter maxabs area img =
    fillArray
      (areaScreen area)
      (areaTopLeft area)
      (areaDelta area)
      (\n x y -> write img n $ color iter $ fractal (x:+y) maxabs iter)
      4

{-# INLINE fillArray #-}
fillArray :: Monad m
  => (Int, Int)
  -> Comp
  -> Comp
  -> (Int -> R -> R -> m ())
  -> Int
  -> m ()
fillArray (w, h) (x1:+y1) (dx:+dy) f d = go 0 0 x1 y1
  where
    n = d * w * h

    go !i !j !x !y
      | i == w    = go 0 j x1 (y+dy)
      | j == n    = return ()
      | otherwise = f j x y >> go (i+1) (j+d) (x+dx) y
