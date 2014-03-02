module Main where

-- TODO: Find replacement to DevIL
-- http://www.haskell.org/haskellwiki/Library/PNG
import Codec.Image.DevIL
import Data.Array.Unsafe
import Fractals.Area
import Fractals.Args
import Fractals.Coloring
import Fractals.Image
import Fractals.Utility
import System.Environment

main :: IO ()
main = do
  ilInit
  (Fractal def iter maxabs area, [path]) <- parseFractal `fmap` getArgs
  arr <- newRgbaArray (areaScreen area)
  measureTime $ fillRgbaArray arr (toRgba ... greyscale) def iter maxabs area
  unsafeFreeze arr >>= writeImage path
