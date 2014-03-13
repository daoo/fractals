{-# LANGUAGE FlexibleContexts #-}
-- |Testing Word8 array write performance for different data types.
module Test (storable, io, st, ptr) where

import Data.Array
import Data.Array.Base (unsafeWrite)
import Data.Array.IO
import Data.Array.ST
import Data.Array.Storable
import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

type Size = (Int, Int)

{-# INLINE size #-}
size :: Size
size = (0, 100)

{-# INLINE fill #-}
fill :: Monad m => Size -> (Int -> Word8 -> m ()) -> m ()
fill (i0, i2) write = go i0
  where
    go i1
      | i1 < i2   = write i1 255 >> go (i1+1)
      | otherwise = return ()

{-# NOINLINE storable #-}
storable :: IO (StorableArray Int Word8)
storable = newArray_ size >>= \arr -> fill size (unsafeWrite arr) >> return arr

{-# NOINLINE io #-}
io :: IO (IOUArray Int Word8)
io = newArray_ size >>= \arr -> fill size (unsafeWrite arr) >> return arr

{-# NOINLINE st #-}
st :: Array Int Word8
st = runSTArray $ newArray_ size >>= \arr -> fill size (unsafeWrite arr) >> return arr

{-# NOINLINE ptr #-}
ptr :: IO (Ptr Word8)
ptr = mallocArray 100 >>= \ptr -> fill size (pokeElemOff ptr) >> return ptr
