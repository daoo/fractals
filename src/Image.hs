{-# LANGUAGE CPP #-}
module Main (main) where

import Codec.Picture.Png
import Codec.Picture.Types
import Data.Vector.Storable (unsafeFreeze)
import Fractals.Area
import Fractals.Args
import Fractals.Coloring
import Fractals.Geometry
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
  v <- newSVector8 size
  measureTime $ fillStorage v greyscale
    (fracDef f) (fracIter f) (fracAbs f) (fracArea f)
  v' <- unsafeFreeze v
  writePng path (Image w h v' :: Image Pixel8)

  where
    size@(Vec w h) = areaScreen $ fracArea f
