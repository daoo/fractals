module Fractals.Storage
  ( newBytePtr
  , newRgbaPtr
  , fillBytePtr
  , fillRgbaPtr
  ) where

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

type Filler c m = (Int -> Int -> c) -> Definition -> Int -> R -> Area -> m ()

{-# INLINE newBytePtr #-}
newBytePtr :: Size -> IO (Ptr Word8)
newBytePtr (Vec w h) = mallocArray $ w * h

{-# INLINE newRgbaPtr #-}
newRgbaPtr :: Size -> IO (Ptr Word8)
newRgbaPtr (Vec w h) = mallocArray $ 4 * w * h

{-# INLINE writeBytePtr #-}
writeBytePtr :: Ptr Word8 -> Int -> Word8 -> IO ()
writeBytePtr = pokeByteOff

{-# INLINE writeRgbaPtr #-}
writeRgbaPtr :: Ptr Word8 -> Int -> RGBA -> IO ()
writeRgbaPtr arr n (RGBA r g b a) = do
  pokeByteOff arr n r
  pokeByteOff arr (n+1) g
  pokeByteOff arr (n+2) b
  pokeByteOff arr (n+3) a

{-# INLINE fillBytePtr #-}
fillBytePtr :: Ptr Word8 -> Filler Word8 IO
fillBytePtr ptr = helper 1 (writeBytePtr ptr)

{-# INLINE fillRgbaPtr #-}
fillRgbaPtr :: Ptr Word8 -> Filler RGBA IO
fillRgbaPtr arr = helper 4 (writeRgbaPtr arr)

{-# INLINE helper #-}
helper :: Monad m
  => Int
  -> (Int -> a -> m ())
  -> Filler a m
helper n f color fractal iter maxabs area = loop
  n
  (areaScreen area)
  (areaTopLeft area)
  (areaDelta area)
  (\i c -> f i $ color iter $ fractal c maxabs iter)
