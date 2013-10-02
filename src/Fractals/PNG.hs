{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Fractals.PNG ( png ) where

import Data.Array
import Data.Array.Base (unsafeAt)
import Data.Bits
import Data.ByteString.Builder
import Data.Monoid
import Data.Word
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString.Lazy as B

hdr :: Builder
hdr = lazyByteString "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A"

chunk :: B.ByteString -> B.ByteString -> Builder
chunk tag xs = word32BE (fromIntegral $ B.length xs) <> lazyByteString dat <> word32BE (calculateCRC dat)
  where
    dat = B.append tag xs

iHDR :: (Word32, Word32) -> Builder
iHDR (w, h) = chunk "IHDR" $ toLazyByteString $ mconcat
  [ word32BE w
  , word32BE h
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
png :: (Word32, Word32) -> [[Bool]] -> Builder
png size dat = hdr <> iHDR size <> iDAT imgbits <> iEND
  where
    imgbits = B.concat $ map scanline dat

scanline :: [Bool] -> B.ByteString
scanline dat = 0 `B.cons` bitpack dat

bitpack :: [Bool] -> B.ByteString
bitpack = go 0 0x80
  where
    go !n !b []     = if b /= 0x80 then B.singleton n else B.empty
    go !n !b (x:xs) = if b == 1
      then v `B.cons` go 0 0x80 xs
      else go v (b `shiftR` 1) xs
      where v = if x then n else n .|. b


calculateCRC :: B.ByteString -> Word32
calculateCRC xs = update 0xffffffff xs `xor` 0xffffffff
  where
    update = B.foldl' step

    step :: Word32 -> Word8 -> Word32
    step crc ch = (tab `unsafeAt` n) `xor` (crc `shiftR` 8)
      where
        n = fromIntegral (crc `xor` fromIntegral ch)

tab :: Array Word8 Word32
tab = listArray (0, 255) $ map (
    times 8 (\c -> if c .&. 1 == 1
                      then 0xedb88320 `xor` (c `shiftR` 1)
                      else c `shiftR` 1)) [0..255]
  where

    times :: Word8 -> (a -> a) -> a -> a
    times  0 _ !x = x
    times !i f !x = times (i-1) f (f x)
