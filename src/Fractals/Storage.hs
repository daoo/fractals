{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
module Fractals.Storage
  ( fillStorage
  , newPtr8
  , newUVector8
  , newSVector8
  , newSVectorRgb8
  , newPtrRgba8
  ) where

import Codec.Picture.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Fractals.Area
import Fractals.Complex
import Fractals.Definitions
import Fractals.Geometry
import Fractals.Render
import qualified Data.Vector.Storable.Mutable as VS
import qualified Data.Vector.Unboxed.Mutable as VU

type Filler pixel m = (Int -> Int -> pixel) -> Definition -> Int -> R -> Area -> m ()

class Pixel a => Storage m s a where
  writeStorage :: s (PixelBaseComponent a) -> Int -> a -> m ()

{-# INLINE newPtr8 #-}
newPtr8 :: Size -> IO (Ptr (PixelBaseComponent Pixel8))
newPtr8 (Vec w h) = mallocArray $ w * h

{-# INLINE newUVector8 #-}
newUVector8 :: Size -> IO (VU.IOVector (PixelBaseComponent Pixel8))
newUVector8 (Vec w h) = VU.unsafeNew $ w * h

{-# INLINE newSVector8 #-}
newSVector8 :: Size -> IO (VS.IOVector (PixelBaseComponent Pixel8))
newSVector8 (Vec w h) = VS.unsafeNew $ w * h

{-# INLINE newSVectorRgb8 #-}
newSVectorRgb8 :: Size -> IO (VS.IOVector (PixelBaseComponent PixelRGB8))
newSVectorRgb8 (Vec w h) = VS.unsafeNew $ 3 * w * h

{-# INLINE newPtrRgba8 #-}
newPtrRgba8 :: Size -> IO (Ptr (PixelBaseComponent PixelRGBA8))
newPtrRgba8 (Vec w h) = mallocArray $ 4 * w * h

instance Storage IO Ptr Pixel8 where
  {-# INLINE writeStorage #-}
  writeStorage = pokeByteOff

instance Storage IO Ptr PixelRGBA8 where
  {-# INLINE writeStorage #-}
  writeStorage p n (PixelRGBA8 r g b a) = do
    pokeByteOff p n r
    pokeByteOff p (n+1) g
    pokeByteOff p (n+2) b
    pokeByteOff p (n+3) a

instance Storage IO VU.IOVector Pixel8 where
  {-# INLINE writeStorage #-}
  writeStorage = VU.unsafeWrite

instance Storage IO VS.IOVector Pixel8 where
  {-# INLINE writeStorage #-}
  writeStorage = VS.unsafeWrite

instance Storage IO VS.IOVector PixelRGB8 where
  {-# INLINE writeStorage #-}
  writeStorage vec n (PixelRGB8 r g b) = do
    VS.unsafeWrite vec n r
    VS.unsafeWrite vec (n+1) g
    VS.unsafeWrite vec (n+2) b

{-# INLINE fillStorage #-}
fillStorage :: forall m s a. (Pixel a, Monad m, Storage m s a)
            => s (PixelBaseComponent a)
            -> Filler a m
fillStorage s = helper n (writeStorage s)
  where
    n = (componentCount :: a -> Int) undefined

{-# INLINE helper #-}
helper :: Monad m
  => Int
  -> (Int -> a -> m ())
  -> Filler a m
helper n f color def iter maxabs area = loop
  n
  (areaScreen area)
  (areaTopLeft area)
  (areaDelta area)
  (\i -> f i . color iter . iterations def maxabs iter)
