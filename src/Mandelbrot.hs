module Main where

import Codec.Image.DevIL
import Fractals.Area
import Fractals.Args
import Fractals.Coloring
import Fractals.Geometry
import Fractals.Storage
import Fractals.Utility

main :: IO ()
main = do
  ilInit
  prog "dist/mandelbrot.png" exMandelbrot

prog :: FilePath -> Fractal -> IO ()
prog path f = do
  ptr <- newRgbaPtr size
  measureTime $ fillRgbaPtr ptr (greyscaleToRGBA ... greyscale)
    (fracDef f) (fracIter f) (fracAbs f) (fracArea f)
  writeImageFromPtr path (h, w) ptr

  where
    size@(Vec w h) = areaScreen $ fracArea f
