{-# LANGUAGE BangPatterns, LambdaCase, MultiWayIf #-}
module Main (main) where

import Control.Concurrent
import Data.Word
import Foreign.Ptr
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
prog !size = newPtr8 size >>= helper
  where
    helper !ptr = go 0 1
      where
        go :: Int -> Int -> IO ()
        go !i !d = do
          fill1 ptr (lerpFractional steps (a, b) i)
          putFrame cells ptr
          threadDelay delay
          let i' = i+d
          if | i' < 0    -> go 0 1
             | i > steps -> go steps (-1)
             | otherwise -> go i' d

    steps, iters, delay, cells :: Int
    !steps = 200
    !iters = 10
    !delay = 30000
    !cells = sizeArea size

    maxabs :: R
    !maxabs = 4

    a, b :: Complex R
    !a = (-2.0) :+ 2.0
    !b = 2.0    :+ (-2.0)

    area = fromAspectCentered size 8 (0 :+ 0)

    fill1 !ptr !c = fillPtr8 ptr (asciiWord8 iters) (julia c) iters maxabs area

{-# INLINE putFrame #-}
-- | Print a "frame" to stdout.
--
-- A newline is printed first, it won't be shown but speeds up the program
-- considerably. Presumably the terminal handles its buffers per line and needs
-- to do lots of reallocation if we continuously write to the same line.
--
-- Also stdout is flushed after the write so that the results are visible immediately.
putFrame :: Int -> Ptr Word8 -> IO ()
putFrame !n !ptr = do
  hPutChar stdout '\n'
  hPutBuf stdout ptr n
  hFlush stdout
