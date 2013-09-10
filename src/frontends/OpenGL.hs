{-# LANGUAGE LambdaCase #-}
module Main (main) where

-- TODO: Implement zooming out
-- TODO: Implement draging
-- TODO: Render with threads and different resolution

import Control.Arrow
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.RWS.Strict hiding (state)
import Fractals.Area
import Fractals.Complex
import Fractals.Utility
import GL.Image
import GL.Shaders
import GL.Util
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

type Point = (Int, Int)
type Size  = (Int, Int)

fixAspectOfRect :: Size -> Point -> Point -> (Point, Point)
fixAspectOfRect winsize (sx, sy) (ex, ey) = (a, (ax + cx, ay + cy))
  where
    -- Find top left and bottom right, avoids problems where you click in the
    -- bottom right corner first and then in the top corner.
    a@(ax, ay) = (min sx ex, min sy ey)
    b          = (max sx ex, max sy ey)

    (cx, cy) = maxRect winsize a b

resizeAreaFromRect :: Area -> Point -> Point -> Area
resizeAreaFromRect area a b = resizePlane a' s area
  where
    a'@(ar :+ ac) = transform a
    (br :+ bc)    = transform b
    s             = (br - ar :+ bc - ac)

    transform = screenToPlane area

texturize :: Image -> IO ()
texturize (Image ptr _ area) =
  GL.texImage2D Nothing GL.NoProxy 0 GL.Luminance8 texSize 0 pixelData
  where
    (w, h)    = areaScreen area
    texSize   = GL.TextureSize2D (fromIntegral w) (fromIntegral h)
    pixelData = GL.PixelData GL.Luminance GL.UnsignedByte ptr

initGL :: IO (GL.Program, GL.Program)
initGL = do
  -- Shaders
  programFractal <- compileAndLink texVertShader texFragShader
  programScreen  <- compileAndLink screenVertShader redFragShader

  -- Framebuffer texture
  _ <- createActiveTexture
  setUniform programFractal "framebuffer" (GL.Index1 (0 :: GL.GLint))

  -- VAO
  let quad :: [GL.GLfloat]
      quad =
        [  1.0, -1.0, 0.0
        ,  1.0,  1.0, 0.0
        , -1.0, -1.0, 0.0
        , -1.0,  1.0, 0.0
        ]

  vao <- createBuffer quad >>= createVAO
  GL.bindVertexArrayObject GL.$= Just vao

  return (programFractal, programScreen)

data Event
  = EventError !GLFW.Error !String
  | EventWindowSize !GLFW.Window !Int !Int
  | EventMouseButton !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos !GLFW.Window !Double !Double
  | EventKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  deriving Show

data Env = Env
  { envEventsChan     :: TQueue Event
  , envWindow         :: !GLFW.Window
  , envProgramFractal :: !GL.Program
  , envProgramScreen  :: !GL.Program
  }

data Mode = Idle | Drag Point | Zoom Point
  deriving Show

data State = State
  { stateMode         :: !Mode
  , stateImage        :: Image
  , stateWindowWidth  :: !Int
  , stateWindowHeight :: !Int
  , stateMouseX       :: !Int
  , stateMouseY       :: !Int
  , stateDirty        :: !Bool
  } deriving Show

stateWindowSize :: State -> Size
stateWindowSize = stateWindowWidth &&& stateWindowHeight

stateMousePos :: State -> Point
stateMousePos = stateMouseX &&& stateMouseY

type Context = RWST Env () State IO

modMode :: (State -> Mode) -> Context ()
modMode f = modify $ \s -> s { stateMode = f s }

modImage :: (Image -> Image) -> Context ()
modImage f = modify $ \s -> s { stateImage = f (stateImage s) }

runContext :: Env -> State -> IO ()
runContext env state = do
  (_, state', _) <- runRWST (adjustWindow >> run) env state
  freeImage (stateImage state')

redraw :: Context ()
redraw = do
  img <- gets stateImage
  liftIO $ update img
  modify $ \s -> s { stateDirty = True }

run :: Context ()
run = do
  env <- ask
  state <- get

  when (stateDirty state) $ do
    liftIO $ texturize (stateImage state)

  liftIO $ do
    GL.clear [ GL.ColorBuffer ]
    GL.currentProgram GL.$= Just (envProgramFractal env)
    GL.drawArrays GL.TriangleStrip 0 4

    case stateMode state of
      Zoom start -> do
        GL.currentProgram GL.$= Just (envProgramScreen env)
        uncurry strokeRectangle $
          fixAspectOfRect
            (stateWindowSize state)
            start
            (stateMousePos state)

      _ -> return ()

    GLFW.swapBuffers (envWindow env)
    GLFW.waitEvents
  processEvents

  q <- liftIO $ GLFW.windowShouldClose (envWindow env)
  unless q run

errorCallback       :: TQueue Event -> GLFW.Error -> String -> IO ()
windowSizeCallback  :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
mouseButtonCallback :: TQueue Event -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback   :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
keyCallback         :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()

errorCallback tc e s                 = atomically $ writeTQueue tc $ EventError e s
windowSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventWindowSize win w h
mouseButtonCallback tc win mb mba mk = atomically $ writeTQueue tc $ EventMouseButton win mb mba mk
cursorPosCallback tc win x y         = atomically $ writeTQueue tc $ EventCursorPos win x y
keyCallback tc win k sc ka mk        = atomically $ writeTQueue tc $ EventKey win k sc ka mk

processEvents :: Context ()
processEvents = do
  tc <- asks envEventsChan
  me <- liftIO $ atomically $ tryReadTQueue tc
  case me of
    Just e -> do
      processEvent e
      processEvents
    Nothing -> return ()

processEvent :: Event -> Context ()
processEvent = \case
  EventError e s -> do
    liftIO $ putStrLn $ "error: " ++ show e ++ " " ++ show s
    quit

  EventWindowSize _ width height -> do
    modify $ \state -> state
      { stateWindowWidth = width
      , stateWindowHeight = height
      }
    adjustWindow

  EventMouseButton _ mb _ _ -> do
    state <- get
    case stateMode state of
      Idle -> case mb of
        GLFW.MouseButton'1 -> dragMode
        GLFW.MouseButton'2 -> zoomMode
        GLFW.MouseButton'3 -> do
          let pos  = stateMousePos state
              area = imageArea $ stateImage state
          liftIO $ do
            print pos
            print $ screenToPlane area pos
        _ -> return ()

      Drag _ -> case mb of
        GLFW.MouseButton'1 -> idleMode -- TODO

        _ -> return ()

      Zoom start -> case mb of
        GLFW.MouseButton'2 -> do
          let area = imageArea $ stateImage state
              size = stateWindowSize state
              end  = stateMousePos state
           in modImage $ setArea $
                uncurry (resizeAreaFromRect area) $
                  fixAspectOfRect size start end
          redraw
          idleMode

        _ -> return ()

  EventCursorPos _ x y ->
    modify $ \s -> s
      { stateMouseX = round x
      , stateMouseY = round y
      }

  EventKey _ k _ ks _ -> case ks of
    GLFW.KeyState'Pressed -> case k of
      GLFW.Key'F1 -> get >>= (liftIO . print)

      GLFW.Key'Escape -> quit
      GLFW.Key'Q      -> quit

      GLFW.Key'I -> modImage (modIter (+10))         >> redraw
      GLFW.Key'D -> modImage (modIter (subtract 10)) >> redraw

      _ -> return ()

    _ -> return ()

  where
    quit = do
      win <- asks envWindow
      liftIO $ GLFW.setWindowShouldClose win True

    idleMode = modMode $ const Idle
    dragMode = modMode $ \s -> Drag (stateMousePos s)
    zoomMode = modMode $ \s -> Zoom (stateMousePos s)

adjustWindow :: Context ()
adjustWindow = do
  env <- ask
  state <- get
  let size@(w, h) = stateWindowSize state

      w' = fromIntegral w
      h' = fromIntegral h

      glpos  = GL.Position 0 0
      glsize = GL.Size w' h'

  liftIO $ do
    setUniform (envProgramScreen env) "size" $ GL.Vertex2 w' h'
    GL.viewport GL.$= (glpos, glsize)

  image <- liftIO $ resize (stateImage state) size
  modify $ \s -> s { stateImage = image }
  redraw

main :: IO ()
main = do
  eventsChan <- newTQueueIO :: IO (TQueue Event)

  let width  = 800
      height = 600

  withWindow 800 600 "Fractals" $ \win -> do
    GLFW.setErrorCallback           $ Just $ errorCallback eventsChan
    GLFW.setWindowSizeCallback win  $ Just $ windowSizeCallback eventsChan
    GLFW.setMouseButtonCallback win $ Just $ mouseButtonCallback eventsChan
    GLFW.setCursorPosCallback win   $ Just $ cursorPosCallback eventsChan
    GLFW.setKeyCallback win         $ Just $ keyCallback eventsChan

    (programDefault, programZoom) <- initGL
    image <- newImage
    update image

    let env = Env
          { envEventsChan     = eventsChan
          , envWindow         = win
          , envProgramFractal = programDefault
          , envProgramScreen  = programZoom
          }

        state = State
          { stateMode         = Idle
          , stateImage        = image
          , stateWindowWidth  = width
          , stateWindowHeight = height
          , stateMouseX       = 0
          , stateMouseY       = 0
          , stateDirty        = True
          }

    runContext env state

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      Just win -> do
        GLFW.makeContextCurrent m
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
      Nothing -> return ()
    GLFW.terminate

  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]
