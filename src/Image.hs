{-# LANGUAGE CPP, LambdaCase #-}
module Main (main) where

import Codec.Picture.Png
import Codec.Picture.Types
import Control.Exception.Base
import Data.Vector.Storable (unsafeFreeze)
import Fractals.Args
import Fractals.Coloring.Palette
import Fractals.Data.Area
import Fractals.Data.Size
import Fractals.Storage

#ifndef MANDELBROT
import System.Environment
#endif

main :: IO ()
#ifdef MANDELBROT
main = prog "dist/mandelbrot.png" exMandelbrot
#else
main = getArgs >>= parseArgs >>= uncurry prog

parseArgs :: [String] -> IO (FilePath, Fractal)
parseArgs = \case
  (path:xs) -> case parseFractal xs of
    Nothing -> error help
    Just f  -> return (path, f)
  _ -> error help

help :: String
help = "fractals-image PATH " ++ usage
#endif

prog :: FilePath -> Fractal -> IO ()
prog path f = do
  colors <- evaluate (mkColorMap colors1)
  ptr    <- newForeignPtr32 size
  fillForeignPtr32 ptr (unsafeColorRgba colors iters) (fracDef f) iters (fracAbs f) area
  vec <- unsafeFreeze $ unsafeToImage ptr (getArea size)
  writePng path (Image (width size) (height size) vec :: Image PixelRGBA8)

  where
    iters = fracIter f

    area = fracArea f
    size = areaScreen area
