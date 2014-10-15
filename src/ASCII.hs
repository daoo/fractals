module Main (main) where

import Data.Char
import Data.Word
import Fractals.Area
import Fractals.Args
import Fractals.Coloring
import Fractals.Math
import Fractals.Storage
import Fractals.Utility
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case parseFractal args of
    Nothing -> putStrLn usage
    Just f  -> prog f

prog :: Fractal -> IO ()
prog f = do
  ptr <- newPtr8 size
  fillStorage ptr (toWord ... ascii)
    (fracDef f) (fracIter f) (fracAbs f) (fracArea f)
  hPutBuf stdout ptr (sizeArea size)
  where
    size = areaScreen $ fracArea f

    toWord :: Char -> Word8
    toWord = fromIntegral . ord
