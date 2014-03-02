module Fractals.Storage
  ( newBytePtr
  , newByteArray
  , newRgbaArray
  , fillByteArray
  , fillBytePtr
  , fillRgbaArray
  ) where

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
import Fractals.Geometry
import Fractals.Render

{-# INLINE newBytePtr #-}
newBytePtr :: Size -> IO (Ptr Word8)
newBytePtr (Vec w h) = mallocArray $ w * h

{-# INLINE newByteArray #-}
newByteArray :: Size -> IO (IOUArray (Int, Int) Word8)
newByteArray (Vec w h) = newArray_ ((0, 0), (h-1, w-1))

{-# INLINE newRgbaArray #-}
newRgbaArray :: Size -> IO (IOUArray (Int, Int, Int) Word8)
newRgbaArray (Vec w h) = newArray_ ((0,0,0), (h-1,w-1, 3))

type Filler c m = (Int -> Int -> c) -> Definition -> Int -> R -> Area -> m ()

{-# INLINE writeBytePtr #-}
writeBytePtr :: Ptr Word8 -> Int -> Word8 -> IO ()
writeBytePtr ptr n = poke (plusPtr ptr n)

{-# INLINE writeRgbaArray #-}
writeRgbaArray :: IOUArray (Int, Int, Int) Word8 -> Int -> RGBA -> IO ()
writeRgbaArray arr n (r, g, b, a) = do
  unsafeWrite arr n r
  unsafeWrite arr (n+1) g
  unsafeWrite arr (n+2) b
  unsafeWrite arr (n+3) a

{-# INLINE fillByteArray #-}
fillByteArray :: IOUArray (Int, Int) Word8 -> Filler Word8 IO
fillByteArray arr = helper 1 (unsafeWrite arr)

{-# INLINE fillBytePtr #-}
fillBytePtr :: Ptr Word8 -> Filler Word8 IO
fillBytePtr ptr = helper 1 (writeBytePtr ptr)

{-# INLINE fillRgbaArray #-}
fillRgbaArray :: IOUArray (Int, Int, Int) Word8 -> Filler RGBA IO
fillRgbaArray arr = helper 4 (writeRgbaArray arr)

{-# INLINE helper #-}
helper :: (Monad m, Color c)
  => Int
  -> (Int -> c -> m ())
  -> Filler c m
helper n f color fractal iter maxabs area = loop
  n
  (areaScreen area)
  (areaTopLeft area)
  (areaDelta area)
  (\i x y -> f i $ color iter $ fractal (x:+y) maxabs iter)
