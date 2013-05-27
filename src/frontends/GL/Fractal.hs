module GL.Fractal
  ( State(..)
  , withState
  , resize
  , modArea
  , modIter
  , update
  ) where

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
  , stateIter :: !Word
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
resize :: IORef State -> (Int, Int) -> IO ()
resize state size = do
  State ptr iter area <- readIORef state
  free ptr
  ptr' <- newGreyscalePtr size
  writeIORef state $ State ptr' iter (resizeScreen size area)

modArea :: (Area -> Area) -> State -> State
modArea f state = state { stateArea = f (stateArea state) }

modIter :: (Word -> Word) -> State -> State
modIter f state = state { stateIter = f (stateIter state) }

-- |Render the fractal and print the time it took
update :: IORef State -> IO ()
update state = do
  State ptr iter area <- readIORef state
  measureTime $ fill ptr greyscale mandelbrot2 iter maxabs area

  where maxabs = 4
