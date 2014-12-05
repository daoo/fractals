{-# LANGUAGE CPP, LambdaCase #-}
module Main (main) where

import Codec.Picture.Png
import Codec.Picture.Types
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
  p <- newForeignPtr32 size
  fillForeignPtr32 p (coloring iters) (fracDef f) iters (fracAbs f) area
  v' <- unsafeFreeze $ unsafeToImage p (getArea size)
  writePng path (Image (width size) (height size) v' :: Image PixelRGBA8)

  where
    iters = fracIter f

    area = fracArea f
    size = areaScreen area

coloring :: Int -> Int -> PackedRGBA
coloring = unsafeColorRgba palette

palette :: ColorMap PackedRGBA
palette = mkColorMap colors1
