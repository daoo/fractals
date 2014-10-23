{-# LANGUAGE BangPatterns, LambdaCase, MultiWayIf #-}
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
    helper !ptr = go 0 1
      where
        go :: Int -> Int -> IO ()
        go !i !d = do
          fill1 ptr (lerpFractional steps (a, b) i)
          put ptr
          threadDelay delay
          let i' = i+d
          if | i' < 0    -> go 0 1
             | i > steps -> go steps (-1)
             | otherwise -> go i' d

    steps, iters, delay, cells :: Int
    !steps = 500
    !iters = 100
    !delay = 10000
    !cells = sizeArea size

    maxabs :: R
    maxabs = 4

    a, b :: Complex R
    !a = (-2.0) :+ 2.0
    !b = 2.0    :+ (-2.0)

    area = fromAspectCentered size 8 (0 :+ 0)

    fill1 !ptr !c = fillPtr8 ptr (toWord . ascii iters) (julia c) iters maxabs area
      where
        toWord :: Char -> Word8
        toWord = fromIntegral . ord

    put ptr = hPutBuf stdout ptr cells
