module Fractals.Storage
  ( newPixel8Ptr
  , newPixel8Vector
  , newPixelRGBA8Ptr
  , fillPixel8Ptr
  , fillPixel8Vector
  , fillPixelRGBA8Ptr
  ) where

import Codec.Picture.Types
import Data.Vector.Unboxed.Mutable
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Fractals.Area
import Fractals.Complex
import Fractals.Definitions
import Fractals.Geometry
import Fractals.Render

type Filler c m = (Int -> Int -> c) -> Definition -> Int -> R -> Area -> m ()

{-# INLINE newPixel8Ptr #-}
newPixel8Ptr :: Size -> IO (Ptr Pixel8)
newPixel8Ptr (Vec w h) = mallocArray $ w * h

newPixel8Vector :: Size -> IO (IOVector (PixelBaseComponent Pixel8))
newPixel8Vector (Vec w h) = unsafeNew $ w * h

{-# INLINE newPixelRGBA8Ptr #-}
newPixelRGBA8Ptr :: Size -> IO (Ptr Pixel8)
newPixelRGBA8Ptr (Vec w h) = mallocArray $ 4 * w * h

{-# INLINE writePixelRGBA8Ptr #-}
writePixelRGBA8Ptr :: Ptr Pixel8 -> Int -> PixelRGBA8 -> IO ()
writePixelRGBA8Ptr arr n (PixelRGBA8 r g b a) = do
  pokeByteOff arr n r
  pokeByteOff arr (n+1) g
  pokeByteOff arr (n+2) b
  pokeByteOff arr (n+3) a

{-# INLINE fillPixel8Ptr #-}
fillPixel8Ptr :: Ptr Pixel8 -> Filler Pixel8 IO
fillPixel8Ptr ptr = helper 1 (pokeByteOff ptr)

{-# INLINE fillPixel8Vector #-}
fillPixel8Vector :: IOVector (PixelBaseComponent Pixel8) -> Filler (PixelBaseComponent Pixel8) IO
fillPixel8Vector vec = helper 1 (unsafeWrite vec)

{-# INLINE fillPixelRGBA8Ptr #-}
fillPixelRGBA8Ptr :: Ptr Pixel8 -> Filler PixelRGBA8 IO
fillPixelRGBA8Ptr arr = helper 4 (writePixelRGBA8Ptr arr)

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
