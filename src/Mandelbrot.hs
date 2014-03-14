module Main where

import Codec.Picture.Png
import Codec.Picture.Types
import Data.Vector.Unboxed (convert, unsafeFreeze)
import Fractals.Area
import Fractals.Args
import Fractals.Coloring
import Fractals.Geometry
import Fractals.Storage
import Fractals.Utility

main :: IO ()
main = prog "dist/mandelbrot.png" exMandelbrot

prog :: FilePath -> Fractal -> IO ()
prog path f = do
  v <- newPixel8Vector size
  measureTime $ fillPixel8Vector v greyscale
    (fracDef f) (fracIter f) (fracAbs f) (fracArea f)
  v' <- unsafeFreeze v
  writePng path (Image w h (convert v') :: Image Pixel8)

  where
    size@(Vec w h) = areaScreen $ fracArea f
