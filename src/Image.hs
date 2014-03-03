module Main (main) where

-- TODO: Find replacement to DevIL
-- http://www.haskell.org/haskellwiki/Library/PNG
import Codec.Image.DevIL
import Data.Array.Unsafe
import Fractals.Area
import Fractals.Args
import Fractals.Coloring
import Fractals.Storage
import Fractals.Utility
import System.Environment

main :: IO ()
main = do
  ilInit
  args <- getArgs
  case args of
    (path:xs) -> case parseFractal xs of
      Nothing -> putStrLn usage
      Just f  -> prog path f
    _ -> putStrLn $ "fractals-image PATH " ++ usage

prog :: FilePath -> Fractal -> IO ()
prog path f = do
  arr <- newRgbaArray (areaScreen $ fracArea f)
  measureTime $ fillRgbaArray arr (toRgba ... greyscale)
    (fracDef f) (fracIter f) (fracAbs f) (fracArea f)
  unsafeFreeze arr >>= writeImage path
