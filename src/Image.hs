module Main (main) where

-- TODO: Find replacement to DevIL
-- http://www.haskell.org/haskellwiki/Library/PNG
import Codec.Image.DevIL
import Fractals.Area
import Fractals.Args
import Fractals.Coloring
import Fractals.Geometry
import Fractals.Storage
import Fractals.Utility
import System.Environment

main :: IO ()
main = do
  ilInit
  args <- getArgs
  case args of
    (path:xs) -> case parseFractal xs of
      Nothing -> printHelp
      Just f  -> prog path f
    _ -> printHelp

printHelp :: IO ()
printHelp = putStrLn $ "fractals-image PATH " ++ usage

prog :: FilePath -> Fractal -> IO ()
prog path f = do
  ptr <- newRgbaPtr size
  measureTime $ fillRgbaPtr ptr (greyscaleToRGBA ... greyscale)
    (fracDef f) (fracIter f) (fracAbs f) (fracArea f)
  writeImageFromPtr path (h, w) ptr

  where
    size@(Vec w h) = areaScreen $ fracArea f
