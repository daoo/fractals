module Fractals.Image
  ( newGreyscalePtr
  , newRgbaArray
  , fillGreyscalePtr
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

{-# INLINE newGreyscalePtr #-}
newGreyscalePtr :: Size -> IO (Ptr Word8)
newGreyscalePtr (Vec w h) = mallocArray $ w * h

{-# INLINE newRgbaArray #-}
newRgbaArray :: Size -> IO (IOUArray (Int, Int, Int) Word8)
newRgbaArray (Vec w h) = newArray_ ((0,0,0), (h-1,w-1, 3))

type Filler c m = (Int -> Int -> c) -> Definition -> Int -> R -> Area -> m ()

writeGreyscalePtr :: Ptr Word8 -> Int -> Greyscale -> IO ()
writeGreyscalePtr ptr n = poke (plusPtr ptr n)

{-# INLINE writeRgbaArray #-}
writeRgbaArray :: IOUArray (Int, Int, Int) Word8 -> Int -> RGBA -> IO ()
writeRgbaArray arr n (r, g, b, a) = do
  unsafeWrite arr n r
  unsafeWrite arr (n+1) g
  unsafeWrite arr (n+2) b
  unsafeWrite arr (n+3) a

{-# INLINE fillGreyscalePtr #-}
fillGreyscalePtr :: Ptr Word8 -> Filler Greyscale IO
fillGreyscalePtr ptr = helper 1 (writeGreyscalePtr ptr)

{-# INLINE fillRgbaArray #-}
fillRgbaArray :: IOUArray (Int, Int, Int) Word8 -> Filler RGBA IO
fillRgbaArray arr = helper 4 (writeRgbaArray arr)

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
