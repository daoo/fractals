{-# LANGUAGE CPP #-}
module Main (main) where

import Data.Char
import Data.Word
import Fractals.Args
import Fractals.Coloring.ASCII
import Fractals.Data.Area
import Fractals.Data.Size
import Fractals.Storage
import System.Environment
import System.IO

main :: IO ()
main = do
#ifdef MANDELBROT
  prog exMandelbrot
#else
  args <- getArgs
  case parseFractal args of
    Nothing -> putStrLn usage
    Just f  -> prog f
#endif

prog :: Fractal -> IO ()
prog f = do
  ptr <- newPtr8 size
  fillPtr8 ptr (toWord . ascii (fracIter f))
    (fracDef f) (fracIter f) (fracAbs f) (fracArea f)
  hPutBuf stdout ptr (getArea size)
  where
    size = areaScreen $ fracArea f

    toWord :: Char -> Word8
    toWord = fromIntegral . ord
