{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Fractals.Storage
  ( newPtr8
  , newPtr32
  , newForeignPtr32
  , fillPtr8
  , fillPtr32
  , fillForeignPtr32
  , unsafeToImage
  ) where

import Data.Complex
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Fractals.Data.Area
import Fractals.Data.Size
import Fractals.Definitions
import qualified Data.Vector.Storable.Mutable as VS

-- TODO: Freeing of allocated storage

{-# INLINE newPtr8 #-}
newPtr8 :: Size -> IO (Ptr Word8)
newPtr8 = mallocArray . getArea

{-# INLINE newPtr32 #-}
newPtr32 :: Size -> IO (Ptr Word32)
newPtr32 = mallocArray . getArea

{-# INLINE newForeignPtr32 #-}
newForeignPtr32 :: Size -> IO (ForeignPtr Word32)
newForeignPtr32 = mallocForeignPtrArray . getArea

type Filler a m = (Int -> a) -> Definition -> Int -> Double -> Area -> m ()

{-# INLINE fillForeignPtr32 #-}
fillForeignPtr32 :: ForeignPtr Word32 -> Filler Word32 IO
fillForeignPtr32 fptr color def iter maxabs area = withForeignPtr fptr (\ptr -> fillPtr32 ptr color def iter maxabs area)

{-# INLINE fillPtr8 #-}
fillPtr8 :: Ptr Word8 -> Filler Word8 IO
fillPtr8 ptr = fill (pokeElemOff ptr)

{-# INLINE fillPtr32 #-}
fillPtr32 :: Ptr Word32 -> Filler Word32 IO
fillPtr32 ptr = fill (pokeElemOff ptr)

{-# INLINE fill #-}
-- |Render a square image using a monadic write function.
--
-- The write function takes an offset into the storage space and a complex
-- number.
fill :: Monad m => (Int -> a -> m ()) -> Filler a m
fill !write !color !def !iter !maxabs !area = outer 0 y1
  where
    !w = width $ areaScreen area

    !(x1:+y1) = areaTopLeft area
    !(dx:+dy) = areaDelta area

    !n = getArea (areaScreen area)

    f !i = write i . color . def (maxabs, iter)

    outer !i !y
      | i < n     = inner i x1 >> outer m (y+dy)
      | otherwise = return ()

        where
          !m = i+w

          inner !j !x
            | j < m     = f j (x:+y) >> inner (j+1) (x+dx)
            | otherwise = return ()

{-# INLINE unsafeToImage #-}
unsafeToImage :: ForeignPtr Word32 -> Int -> VS.MVector s Word8
unsafeToImage ptr n = VS.unsafeFromForeignPtr0 (castForeignPtr ptr) (n*4)
