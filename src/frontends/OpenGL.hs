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
import Graphics.Rendering.OpenGL as GL hiding (get)
import qualified Graphics.UI.GLFW as GLFW

calcZoomRectEnd :: Integral n => (n, n) -> (n, n) -> (n, n) -> (n, n)
calcZoomRectEnd size start end =
  start `add` maxRect size start end

texturize :: Image -> IO ()
texturize (Image ptr _ area) =
  texImage2D Nothing NoProxy 0 Luminance8 texSize 0 pixelData
  where
    (w, h) = areaScreen area
    texSize   = TextureSize2D (fromIntegral w) (fromIntegral h)
    pixelData = PixelData Luminance UnsignedByte ptr

initGL :: IO (Program, Program)
initGL = do
  -- Shaders
  programFractal <- compileAndLink texVertShader texFragShader
  programScreen  <- compileAndLink screenVertShader redFragShader

  -- Framebuffer texture
  [tex] <- genObjectNames 1
  activeTexture            $= TextureUnit 0
  textureBinding Texture2D $= Just tex
  textureFilter Texture2D  $= ((Nearest, Nothing), Nearest)
  rowAlignment Unpack      $= 1
  setUniform programFractal "framebuffer" (Index1 (0 :: GLint))

  -- VAO
  let quad :: [GLfloat]
      quad =
        [  1.0, -1.0, 0.0
        ,  1.0,  1.0, 0.0
        , -1.0, -1.0, 0.0
        , -1.0,  1.0, 0.0
        ]

  vao <- createBuffer quad >>= createVAO
  bindVertexArrayObject $= Just vao

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

data Mode = Idle | Drag (Int, Int) | Zoom (Int, Int)
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

stateWindowSize :: State -> (Int, Int)
stateWindowSize = stateWindowWidth &&& stateWindowHeight

stateMousePos :: State -> (Int, Int)
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
    clear [ ColorBuffer ]
    currentProgram $= Just (envProgramFractal env)
    drawArrays TriangleStrip 0 4

    case stateMode state of
      Zoom start -> do
        currentProgram $= Just (envProgramScreen env)
        strokeRectangle start $ calcZoomRectEnd
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
          let size = stateWindowSize state
              pos  = stateMousePos state
              end  = calcZoomRectEnd size start pos
              area = imageArea $ stateImage state

              -- TODO: Encapsulate this code in a function
              f          = screenToPlane area
              a@(sr:+si) = f start
              (er:+ei)   = f end
              s          = (er - sr) :+ (si - ei)
              area'      = resizePlane a s area

          modImage $ setArea area'
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
    setUniform (envProgramScreen env) "size" $ Vertex2 w' h'
    GL.viewport $= (glpos, glsize)

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
