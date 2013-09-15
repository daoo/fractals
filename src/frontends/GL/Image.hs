-- TODO: Rename
module GL.Image
  ( Image(..)
  , newImage
  , freeImage
  , resize
  , setArea
  , modIter
  , update
  ) where

import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Geometry
import Fractals.Image
import Fractals.Utility

data Image = Image
  { imagePtr :: Ptr Word8
  , imageIter :: !Int
  , imageArea :: Area
  } deriving Show

newImage :: IO Image
newImage = do
  ptr <- newGreyscalePtr defsize
  return $ Image ptr iter area
  where
    defsize = Vec 800 600
    iter    = 100
    area    = fromAspectCentered defsize 4.3 (0:+0)

freeImage :: Image -> IO ()
freeImage = free . imagePtr

-- |Resize the storage
-- Reallocate the storage to accustom the new size, does not render the
-- frectal.
resize :: Image -> Size -> IO Image
resize (Image ptr iter area) size = do
  free ptr
  ptr' <- newGreyscalePtr size
  return $ Image ptr' iter (resizeScreen size area)

setArea :: Area -> Image -> Image
setArea area img = img { imageArea = area }

modIter :: (Int -> Int) -> Image -> Image
modIter f img = img { imageIter = clampLow 1 $ f (imageIter img) }

-- |Render the fractal and print the time it took
update :: Image -> IO ()
update (Image ptr iter area) = measureTime $
  fill ptr greyscale mandelbrot2 iter maxabs area
  where
    maxabs = 4
