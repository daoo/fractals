{-# LANGUAGE OverloadedStrings, BangPatterns, FlexibleContexts #-}
module Fractals.PNG ( png ) where

import Data.Array.Base (unsafeAt, unsafeRead)
import Data.Array.IArray
import Data.Array.IO
import Data.Bits
import Data.ByteString.Builder
import Data.Monoid
import Data.Word
import System.IO
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString.Lazy as B

hdr :: Builder
hdr = lazyByteString "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A"

chunk :: B.ByteString -> B.ByteString -> Builder
chunk tag xs = word32BE (fromIntegral $ B.length xs) <> lazyByteString dat <> word32BE (calculateCRC dat)
  where
    dat = B.append tag xs

iHDR :: (Int, Int) -> Builder
iHDR (w, h) = chunk "IHDR" $ toLazyByteString $ mconcat
  [ word32BE $ fromIntegral w
  , word32BE $ fromIntegral h
  , bitDepth
  , colorType
  , compressionMethod
  , filterMethod
  , interlaceMethod
  ] where
    bitDepth          = word8 1
    colorType         = word8 0
    compressionMethod = word8 0
    filterMethod      = word8 0
    interlaceMethod   = word8 0

iDAT :: B.ByteString -> Builder
iDAT = chunk "IDAT" . Z.compress

iEND :: Builder
iEND = chunk "IEND" B.empty

-- | Return a monochrome PNG file from a two dimensional bitmap
-- stored in a list of lines represented as a list of booleans.
png :: FilePath -> (Int, Int) -> IOUArray (Int, Int) Word8 -> IO ()
png path size@(w, h) dat = do
  h <- openBinaryFile path ReadMode
  hPutBuilder h (hdr <> iHDR size <> iDAT (toLazyByteString imgbits) <> iEND)
  hClose h
  where
    imgbits = goy 0

    goy j | j < h    = word8 0 <> gox j 0 <> goy (j+1)
          | otherwise = mempty

    gox j i | i < w     = word8 (dat ! (i, j)) <> gox j (i+1)
            | otherwise = mempty

calculateCRC :: B.ByteString -> Word32
calculateCRC xs = update 0xffffffff xs `xor` 0xffffffff
  where
    update = B.foldl' step

    step :: Word32 -> Word8 -> Word32
    step crc ch = (tab `unsafeAt` n) `xor` (crc `shiftR` 8)
      where
        n = fromIntegral (crc `xor` fromIntegral ch)

tab :: Array Word8 Word32
tab = listArray (0, 255) $ flip map [0..255] (\n ->
    times 8 (\c -> if c .&. 1 == 1
                      then 0xedb88320 `xor` (c `shiftR` 1)
                      else c `shiftR` 1) n)
  where

    times :: Word8 -> (a -> a) -> a -> a
    times  0 _ !x = x
    times !i f !x = times (i-1) f (f x)
