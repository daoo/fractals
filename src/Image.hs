{-# LANGUAGE CPP #-}
module Main (main) where

import Codec.Picture.Png
import Codec.Picture.Types
import Data.Vector.Storable (unsafeFreeze)
import Fractals.Args
import Fractals.Coloring.Palette
import Fractals.Data.Area
import Fractals.Data.Size
import Fractals.Storage
import Fractals.Utility

#ifndef MANDELBROT
import System.Environment
#endif

main :: IO ()
main = getFractal >>= prog

getFractal :: IO (FilePath, Fractal)
#ifdef MANDELBROT
getFractal = return ("dist/mandelbrot.png", exMandelbrot)
#else
getFractal = do
  args <- getArgs
  case args of
    (path:xs) -> case parseFractal xs of
      Nothing -> error help
      Just f  -> return (path, f)
    _ -> error help

help :: String
help = "fractals-image PATH " ++ usage
#endif

prog :: (FilePath, Fractal) -> IO ()
prog (path, f) = do
  p <- newForeignPtr32 size
  measureTime $ fillForeignPtr32 p
    (coloring (fracIter f))
    (fracDef f)
    (fracIter f)
    (fracAbs f)
    (fracArea f)
  v' <- unsafeFreeze $ unsafeToImage p (getArea $ areaScreen $ fracArea f)
  writePng path (Image (width size) (height size) v' :: Image PixelRGBA8)

  where
    size = areaScreen $ fracArea f

coloring :: Int -> Int -> PackedRGBA
coloring = unsafeColorRgba palette

palette :: ColorMap PackedRGBA
palette = mkColorMap colors1
