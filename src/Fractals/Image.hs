{-# LANGUAGE TypeOperators #-}
module Fractals.Image
  ( writeFractal
  ) where

import Data.Array.Repa (Z(..), (:.)(..))
import Data.Array.Repa.IO.DevIL
import Fractals.Coloring
import Fractals.Fractal
import qualified Data.Array.Repa as R

writeFractal :: Fractal -> FilePath -> IO ()
writeFractal fractal fp = write $ R.fromFunction (Z :. h :. w) f
  where
    (w, h) = fractalScreen fractal
    func   = mkFunction fractal

    f (Z :. y :. x) = greyscale (fractalIter fractal) $ func x y

    write = runIL . writeImage fp . Grey . R.computeS
