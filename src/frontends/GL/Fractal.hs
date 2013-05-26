module GL.Fractal where

import Data.IORef
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Image
import Fractals.Utility

data State = State
  { stateImg :: Ptr Word8
  , stateIter :: !Int
  , stateArea :: Area
  } deriving Show

-- |Run an IO action with a State
-- Handles the memory, does not render the fractal.
withState :: (IORef State -> IO ()) -> IO ()
withState f = do
  let defsize = (800, 600)
      iter = 100
      area = fromAspectCentered defsize 4.3 (0:+0)
  ptr <- newGreyscalePtr defsize
  state <- newIORef $ State ptr iter area
  f state
  readIORef state >>= (free . stateImg)

-- |Resize the storage
-- Reallocate the storage to accustom the new size, does not render the
-- frectal.
resizeState :: IORef State -> (Int, Int) -> IO ()
resizeState state size = do
  State ptr iter area <- readIORef state
  free ptr
  ptr' <- newGreyscalePtr size
  writeIORef state $ State ptr' iter (resizeScreen size area)

modAreaState :: IORef State -> (Area -> Area) -> IO ()
modAreaState state f = modifyIORef state (\s ->
  s { stateArea = f (stateArea s) })

-- |Render the fractal and print the time it took
updateState :: IORef State -> IO ()
updateState state = do
  State ptr iter area <- readIORef state
  measureTime $ fill ptr greyscale mandelbrot2 iter maxabs area

  where maxabs = 4
