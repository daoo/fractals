{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main (main) where

-- TODO: Render in chunks so we can move around quickly
-- TODO: Fix window size changes in the zoom stack
-- TODO: Render with threads and different resolution

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.RWS.Strict hiding (state)
import Data.Array.Storable
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Math
import Fractals.Storage
import Fractals.Utility
import qualified Data.ByteString as BS
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

-- {{{ Utility
resizeAreaFromRect :: Area -> Rectangle -> Area
resizeAreaFromRect area (Rectangle a b) = resizePlane a' s area
  where
    a'@(ar :+ ac) = transform a
    (br :+ bc)    = transform b
    s             = abs (br - ar) :+ abs (bc - ac)

    transform = screenToPlane area
-- }}}
-- {{{ Image
data Image = Image
  { imagePtr :: Ptr Word8
  , imageIter :: !Int
  , imageArea :: Area
  } deriving Show

newImage :: IO Image
newImage = do
  ptr <- newPtr8 defsize
  return $ Image ptr iter area
  where
    defsize = mkSize 800 600
    iter    = 100
    area    = fromAspectCentered defsize 4.3 (0:+0)

freeImage :: Image -> IO ()
freeImage = free . imagePtr

-- |Resize the storage
-- Reallocate the storage to accustom the new size, does not render the
-- frectal.
resizeImage :: Image -> Size -> IO Image
resizeImage (Image ptr iter area) size = do
  free ptr
  ptr' <- newPtr8 size
  return $ Image ptr' iter (resizeScreen size area)

setArea :: Area -> Image -> Image
setArea area img = img { imageArea = area }

modifyIterations :: (Int -> Int) -> Image -> Image
modifyIterations f img = img { imageIter = clampLow 1 $ f (imageIter img) }

-- |Render the fractal and print the time it took
updateImage :: Image -> IO ()
updateImage (Image ptr iter area) = measureTime $
  fillStorage ptr (greyscale iter) mandelbrot2 iter maxabs area
  where
    maxabs = 4
-- }}}
-- {{{ Generic OpenGL
createShader :: GL.ShaderType -> BS.ByteString -> IO GL.Shader
createShader t src = do
  shader <- GL.createShader t
  GL.shaderSourceBS shader GL.$= src
  GL.compileShader shader
  ok <- GL.get (GL.compileStatus shader)
  unless ok $ do
    infoLog <- GL.get (GL.shaderInfoLog shader)
    putStrLn "Shader error:"
    putStrLn infoLog
    putStrLn ""
    GL.deleteObjectName shader
    ioError (userError "shader compilation failed")
  return shader

createProgram :: GL.Shader -> GL.Shader -> IO GL.Program
createProgram vs fs = do
  prog <- GL.createProgram
  GL.attachShader prog vs
  GL.attachShader prog fs
  GL.deleteObjectName vs
  GL.deleteObjectName fs
  return prog

compileAndLink :: BS.ByteString -> BS.ByteString -> IO GL.Program
compileAndLink vert frag = do
  vs <- createShader GL.VertexShader vert
  fs <- createShader GL.FragmentShader frag
  prog <- createProgram vs fs
  GL.bindFragDataLocation prog "position" GL.$= 0
  GL.attribLocation prog "fragmentColor"  GL.$= GL.AttribLocation 0
  checkedLinkProgram prog
  return prog

checkedLinkProgram :: GL.Program -> IO ()
checkedLinkProgram prog = do
  GL.linkProgram prog
  ok <- GL.get (GL.linkStatus prog)
  unless ok $ do
    GL.get (GL.programInfoLog prog) >>= putStrLn
    GL.deleteObjectName prog
    ioError (userError "program linking failed")

setUniform :: GL.Uniform a => GL.Program -> String -> a -> IO ()
setUniform prog var val = do
  GL.currentProgram GL.$= Just prog
  location <- GL.get (GL.uniformLocation prog var)
  GL.uniform location GL.$= val

createBuffer :: Storable a => [a] -> IO GL.BufferObject
createBuffer xs = do
  let c = length xs
      n = fromIntegral $ c * sizeOf (head xs)
  buffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
  newListArray (0, c-1) xs >>= (`withStorableArray` \ptr ->
    GL.bufferData GL.ArrayBuffer GL.$= (n, ptr, GL.StaticDraw))
  GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
  return buffer

createVAO :: GL.BufferObject -> IO GL.VertexArrayObject
createVAO buffer = do
  let attrib = GL.AttribLocation 0
  vao <- GL.genObjectName
  GL.bindVertexArrayObject      GL.$= Just vao
  GL.bindBuffer GL.ArrayBuffer  GL.$= Just buffer
  GL.vertexAttribArray attrib   GL.$= GL.Enabled
  GL.vertexAttribPointer attrib GL.$= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)
  GL.bindBuffer GL.ArrayBuffer  GL.$= Nothing
  GL.bindVertexArrayObject      GL.$= Nothing
  return vao

createActiveTexture :: IO GL.TextureObject
createActiveTexture = do
  tex <- GL.genObjectName
  GL.activeTexture               GL.$= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D GL.$= Just tex
  GL.textureFilter GL.Texture2D  GL.$= ((GL.Nearest, Nothing), GL.Nearest)
  GL.rowAlignment GL.Unpack      GL.$= 1
  return tex
-- }}}
-- {{{ Shaders
texFragShader :: BS.ByteString
texFragShader =
  "#version 130\n\

  \precision highp float;\n\

  \in  vec2 texCoord;\n\
  \out vec4 fragmentColor;\n\

  \uniform sampler2D framebuffer;\n\

  \void main() {\n\
  \  fragmentColor = texture2D(framebuffer, texCoord);\n\
  \}"

texVertShader :: BS.ByteString
texVertShader =
  "#version 130\n\

  \in  vec3 position;\n\
  \out vec2 texCoord;\n\

  \void main() {\n\
  \  texCoord    = (position.xy + vec2(1.0)) * vec2(0.5, -0.5);\n\
  \  gl_Position = vec4(position, 1);\n\
  \}"

redFragShader :: BS.ByteString
redFragShader =
  "#version 130\n\
  \precision highp float;\n\
  \out vec4 fragmentColor;\n\
  \void main() {\n\
  \  fragmentColor = vec4(1, 0, 0, 1);\n\
  \}"

screenVertShader :: BS.ByteString
screenVertShader =
  "#version 130\n\

  \in vec3 position;\n\
  \uniform ivec2 size;\n\

  \void main() {\n\
  \  gl_Position = vec4((vec2(position) / size) * vec2(2, -2) + vec2(-1, 1), 0, 1);\n\
  \}"
-- }}}
-- {{{ Fractals OpenGL
texturize :: Image -> IO ()
texturize (Image ptr _ area) =
  GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.Luminance8 texSize 0 pixelData
  where
    size      = areaScreen area
    w         = width size
    h         = height size
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

strokeRectangle :: Rectangle -> IO ()
strokeRectangle (Rectangle (Vec x1 y1) (Vec x2 y2)) =
  GL.renderPrimitive GL.LineLoop $ do
    GL.color $ GL.Color3 (1.0 :: GL.GLfloat) 0 0
    GL.vertex $ GL.Vertex2 (fromIntegral x1 :: GL.GLfloat) (fromIntegral y1 :: GL.GLfloat)
    GL.vertex $ GL.Vertex2 (fromIntegral x2 :: GL.GLfloat) (fromIntegral y1 :: GL.GLfloat)
    GL.vertex $ GL.Vertex2 (fromIntegral x2 :: GL.GLfloat) (fromIntegral y2 :: GL.GLfloat)
    GL.vertex $ GL.Vertex2 (fromIntegral x1 :: GL.GLfloat) (fromIntegral y2 :: GL.GLfloat)
-- }}}
-- {{{ Events
data Event
  = EventError !GLFW.Error !String
  | EventWindowSize !GLFW.Window !Int !Int
  | EventMouseButton !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos !GLFW.Window !Double !Double
  | EventKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  deriving Show

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
-- }}}
-- {{{ Program Logic
data Env = Env
  { envEventsChan     :: TQueue Event
  , envWindow         :: !GLFW.Window
  , envProgramFractal :: !GL.Program
  , envProgramScreen  :: !GL.Program
  }

data Mode = Idle | Zoom Point
  deriving Show

data State = State
  { stateMode         :: !Mode
  , stateImage        :: Image
  , stateAreaStack    :: [Area]
  , stateWindowWidth  :: !Word
  , stateWindowHeight :: !Word
  , stateMouseX       :: !Int
  , stateMouseY       :: !Int
  , stateDirty        :: !Bool
  } deriving Show

stateWindowSize :: State -> Size
stateWindowSize s = mkSize (stateWindowWidth s) (stateWindowHeight s)

stateMousePos :: State -> Point
stateMousePos s = Vec (stateMouseX s) (stateMouseY s)

type Context = RWST Env () State IO

redraw :: Context ()
redraw = do
  img <- gets stateImage
  lift $ updateImage img
  modify $ \s -> s { stateDirty = True }

pushArea :: Area -> Context ()
pushArea area = do
  modify $ \s -> s
    { stateImage     = setArea area (stateImage s)
    , stateAreaStack = imageArea (stateImage s) : stateAreaStack s
    }
  redraw

popArea :: Context ()
popArea = do
  stack <- gets stateAreaStack
  case stack of
    []     -> return ()
    (x:xs) -> do
      modify $ \s -> s
        { stateImage     = setArea x (stateImage s)
        , stateAreaStack = xs
        }
      redraw

changeIter :: (Int -> Int) -> Context ()
changeIter f = do
  modify $ \s -> s
    { stateImage = modifyIterations f (stateImage s) }
  redraw

modMode :: (State -> Mode) -> Context ()
modMode f = modify $ \s -> s { stateMode = f s }

runContext :: Env -> State -> IO ()
runContext env state = do
  (_, state', _) <- runRWST (adjustWindow >> run) env state
  freeImage (stateImage state')

run :: Context ()
run = do
  env <- ask
  state <- get

  when (stateDirty state) $
    lift $ texturize (stateImage state)

  lift $ do
    GL.clear [ GL.ColorBuffer ]
    GL.currentProgram GL.$= Just (envProgramFractal env)
    GL.drawArrays GL.TriangleStrip 0 4

    case stateMode state of
      Zoom start -> do
        GL.currentProgram GL.$= Just (envProgramScreen env)
        strokeRectangle $
          fixAspect
            (stateWindowSize state)
            start
            (stateMousePos state)

      _ -> return ()

    GLFW.swapBuffers (envWindow env)
    GLFW.waitEvents
  processEvents

  q <- lift $ GLFW.windowShouldClose (envWindow env)
  unless q run

processEvents :: Context ()
processEvents = do
  tc <- asks envEventsChan
  me <- lift $ atomically $ tryReadTQueue tc
  case me of
    Just e -> do
      processEvent e
      processEvents
    Nothing -> return ()

processEvent :: Event -> Context ()
processEvent = \case
  EventError e s -> do
    lift $ putStrLn $ "error: " ++ show e ++ " " ++ show s
    quit

  EventWindowSize _ w h -> do
    modify $ \state -> state
      { stateWindowWidth  = fromIntegral w
      , stateWindowHeight = fromIntegral h
      }
    adjustWindow

  EventMouseButton _ mb s _ -> do
    state <- get
    case (s, mb, stateMode state) of

      (GLFW.MouseButtonState'Pressed  , GLFW.MouseButton'1 , Idle)       -> recenter
      (GLFW.MouseButtonState'Pressed  , GLFW.MouseButton'3 , Idle)       -> printInfo
      (GLFW.MouseButtonState'Pressed  , GLFW.MouseButton'2 , Idle)       -> zoomMode
      (GLFW.MouseButtonState'Released , GLFW.MouseButton'2 , Zoom start) -> zoom start
      (GLFW.MouseButtonState'Pressed  , _                  , Zoom _)     -> idleMode
      (_                              , _                  , _)          -> return ()

  EventCursorPos _ x y ->
    modify $ \s -> s
      { stateMouseX = round x
      , stateMouseY = round y
      }

  EventKey _ k _ ks _ -> case ks of
    GLFW.KeyState'Pressed -> case k of
      GLFW.Key'F1 -> get >>= (lift . print)

      GLFW.Key'Escape -> quit
      GLFW.Key'Q      -> quit

      GLFW.Key'I -> changeIter (+100)
      GLFW.Key'D -> changeIter (subtract 100)

      GLFW.Key'Up -> popArea

      _ -> return ()

    _ -> return ()

  where
    quit = do
      win <- asks envWindow
      lift $ GLFW.setWindowShouldClose win True

    recenter = do
      state <- get
      let area = imageArea $ stateImage state
          pos  = stateMousePos state
       in pushArea $ setPlaneCenter (screenToPlane area pos) area
      idleMode

    zoom start = do
      state <- get
      let area = imageArea $ stateImage state
          size = stateWindowSize state
          end  = stateMousePos state
        in pushArea $ resizeAreaFromRect area $ fixAspect size start end
      idleMode

    printInfo = do
      state <- get
      let pos  = stateMousePos state
          area = imageArea $ stateImage state
      lift $ do
        print pos
        print $ screenToPlane area pos

    idleMode = modMode $ const Idle
    zoomMode = modMode $ \s -> Zoom (stateMousePos s)

adjustWindow :: Context ()
adjustWindow = do
  env <- ask
  state <- get
  let size = stateWindowSize state

      w = fromIntegral $ width size
      h = fromIntegral $ height size

      glpos  = GL.Position 0 0
      glsize = GL.Size w h

  lift $ do
    setUniform (envProgramScreen env) "size" $ GL.Vertex2 w h
    GL.viewport GL.$= (glpos, glsize)

  image <- lift $ resizeImage (stateImage state) size
  modify $ \s -> s { stateImage = image }
  redraw
-- }}}
-- {{{ Main
main :: IO ()
main = do
  eventsChan <- newTQueueIO :: IO (TQueue Event)

  let w = 800 :: Word
      h = 600 :: Word

  withWindow (fromIntegral w) (fromIntegral h) "Fractals" $ \win -> do
    GLFW.setErrorCallback           $ Just $ errorCallback eventsChan
    GLFW.setWindowSizeCallback win  $ Just $ windowSizeCallback eventsChan
    GLFW.setMouseButtonCallback win $ Just $ mouseButtonCallback eventsChan
    GLFW.setCursorPosCallback win   $ Just $ cursorPosCallback eventsChan
    GLFW.setKeyCallback win         $ Just $ keyCallback eventsChan

    (programDefault, programZoom) <- initGL
    image <- newImage
    updateImage image

    let env = Env
          { envEventsChan     = eventsChan
          , envWindow         = win
          , envProgramFractal = programDefault
          , envProgramScreen  = programZoom
          }

        state = State
          { stateMode         = Idle
          , stateImage        = image
          , stateAreaStack    = [imageArea image]
          , stateWindowWidth  = w
          , stateWindowHeight = h
          , stateMouseX       = 0
          , stateMouseY       = 0
          , stateDirty        = True
          }

    runContext env state

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow w h title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    m <- GLFW.createWindow w h title Nothing Nothing
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
-- }}}

-- vim:set fdm=marker:
