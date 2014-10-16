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
prog size = do
  ptr <- newPtr8 size
  mapM_ (\f -> fill ptr f >> put ptr >> threadDelay 10000) fractals
  where
    n = sizeArea size

    area = fromAspectCentered size 8 (0 :+ 0)

    toWord :: Char -> Word8
    toWord = fromIntegral . ord

    fill ptr c = fillStorage ptr (toWord . ascii 100)
      (julia c)
      100
      4
      area

    put ptr = hPutBuf stdout ptr n

    xs = let a = [-2.2, -2.19 .. 1.2] in cycle (a ++ reverse a)
    ys = repeat 0.1554

    fractals = zipWith (:+) xs ys
