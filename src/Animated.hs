{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Concurrent
import Data.Char
import Data.Word
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Math
import Fractals.Storage
import System.Environment
import System.IO

main :: IO ()
main = getArgs >>= \case
  [w, h] -> prog $ mkSize (read w) (read h)
  _      -> error "Usage: fractals-ascii-animated WIDTH HEIGHT"

prog :: Size -> IO ()
prog size = newPtr8 size >>= helper
  where
    helper ptr = go 0 1
      where
        go i d
          | i <= 0    = go 0 1
          | i > steps = go steps (-1)
          | otherwise = do
            fill ptr (lerpFractional steps (a, b) i)
            put ptr
            threadDelay (round $ delay * 1000000.0)
            go (i+d) d

    steps = 100
    iters = 100
    fps   = 100
    delay = (1.0 :: Double) / (realToFrac (fps :: Int))
    cells = sizeArea size

    a = (-2.0) :+ 2.0
    b = 2.0    :+ (-2.0)

    area = fromAspectCentered size 8 (0 :+ 0)

    fill ptr c = fillStorage ptr (toWord . ascii 100) (julia c) iters 4 area
      where
        toWord :: Char -> Word8
        toWord = fromIntegral . ord

    put ptr = hPutBuf stdout ptr cells
