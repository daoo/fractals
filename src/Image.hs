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
import System.Environment

main :: IO ()
main = do
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
  v <- newSVector8 size
  measureTime $ fillStorage v greyscale
    (fracDef f) (fracIter f) (fracAbs f) (fracArea f)
  v' <- unsafeFreeze v
  writePng path (Image w h v' :: Image Pixel8)

  where
    size@(Vec w h) = areaScreen $ fracArea f
