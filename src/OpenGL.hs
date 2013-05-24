module Main where

import Control.Monad
import Data.Array.Storable
import Data.IORef
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Image
import Fractals.Utility
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

-- {{{ State
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
      area = aspectCentered defsize 4.3 (0:+0)
  ptr <- newGreyscalePtr defsize
  state <- newIORef $ State ptr iter area
  f state
  readIORef state >>= (free . stateImg)

-- |Resize the storage
-- Reallocate the storage to accustom the new size, does not render the
-- frectal.
reallocSize :: IORef State -> (Int, Int) -> IO ()
reallocSize state size = do
  State ptr iter area <- readIORef state
  free ptr
  ptr' <- newGreyscalePtr size
  writeIORef state $ State ptr' iter (resizeScreen size area)

-- |Render the fractal and print the time it took
update :: IORef State -> IO ()
update state = do
  State ptr iter area <- readIORef state
  measureTime $ fill ptr greyscale mandelbrot2 iter maxabs area

  where maxabs = 4
-- }}}
-- {{{ OpenGL helpers
createShader :: Shader s => String -> IO s
createShader src = do
  [shader] <- genObjectNames 1
  shaderSource shader $= [src]
  compileShader shader
  ok <- get (compileStatus shader)
  unless ok $ do
    infoLog <- get (shaderInfoLog shader)
    putStrLn $ unlines ["Shader error:", infoLog, ""]
    deleteObjectNames [shader]
    ioError (userError "shader compilation failed")
  return shader

createProgram :: VertexShader -> FragmentShader -> IO Program
createProgram vs fs = do
  [prog] <- genObjectNames 1
  attachedShaders prog $= ([vs], [fs])
  deleteObjectNames [vs]
  deleteObjectNames [fs]
  return prog

createBuffer :: Storable a => [a] -> IO BufferObject
createBuffer xs = do
  let c = length xs
      n = fromIntegral $ c * sizeOf (head xs)
  [buffer] <- genObjectNames 1
  bindBuffer ArrayBuffer $= Just buffer
  newListArray (0, c-1) xs >>= (`withStorableArray` \ptr ->
    bufferData ArrayBuffer $= (n, ptr, StaticDraw))
  bindBuffer ArrayBuffer $= Nothing
  return buffer

createVAO :: BufferObject -> IO VertexArrayObject
createVAO buffer = do
  let attrib = AttribLocation 0
  [vao] <- genObjectNames 1
  bindVertexArrayObject      $= Just vao
  bindBuffer ArrayBuffer     $= Just buffer
  vertexAttribArray attrib   $= Enabled
  vertexAttribPointer attrib $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
  bindBuffer ArrayBuffer     $= Nothing
  bindVertexArrayObject      $= Nothing
  return vao

checkedLinkProgram :: Program -> IO ()
checkedLinkProgram prog = do
  linkProgram prog
  ok <- get (linkStatus prog)
  unless ok $ do
    get (programInfoLog prog) >>= putStrLn
    deleteObjectNames [prog]
    ioError (userError "program linking failed")

strokeRectangle :: VertexComponent a => (a, a) -> (a, a) -> IO ()
strokeRectangle (x1, y1) (x2, y2) = do
  renderPrimitive LineLoop $ do
    color $ (Color3 (1.0::GLfloat) 0 0)
    vertex $ (Vertex2 x1 y1)
    vertex $ (Vertex2 x2 y1)
    vertex $ (Vertex2 x2 y2)
    vertex $ (Vertex2 x1 y2)
-- }}}
-- {{{ Shaders
texFragShader :: String
texFragShader =
  "#version 130\n\

  \precision highp float;\n\

  \in  vec2 texCoord;\n\
  \out vec4 fragmentColor;\n\

  \uniform sampler2D framebuffer;\n\

  \void main() {\n\
  \  fragmentColor = texture2D(framebuffer, texCoord);\n\
  \}"

texVertShader :: String
texVertShader =
  "#version 130\n\

  \in  vec3 position;\n\
  \out vec2 texCoord;\n\

  \void main() {\n\
  \  texCoord    = (position.xy + vec2(1.0)) * vec2(0.5, -0.5);\n\
  \  gl_Position = vec4(position, 1);\n\
  \}"

redFragShader :: String
redFragShader =
  "#version 130\n\
  \precision highp float;\n\
  \out vec4 fragmentColor;\n\
  \void main() {\n\
  \  fragmentColor = vec4(1, 0, 0, 1);\n\
  \}"

screenVertShader :: String
screenVertShader =
  "#version 130\n\

  \in vec3 position;\n\
  \uniform vec2 size;\n\

  \void main() {\n\
  \  gl_Position = vec2(2.0) * position.xy / size - vec2(1.0);\n\
  \}"
-- }}}
-- {{{ Init GL
initGL :: IO Program
initGL = do
  -- Shaders
  vs <- createShader texVertShader
  fs <- createShader texFragShader
  prog <- createProgram vs fs
  bindFragDataLocation prog "position" $= 0
  attribLocation prog "fragmentColor"  $= AttribLocation 0
  checkedLinkProgram prog
  let setUniform var val = do
        location <- get (uniformLocation prog var)
        uniform location $= val
  currentProgram $= Just prog

  -- Framebuffer texture
  [tex] <- genObjectNames 1
  activeTexture            $= TextureUnit 0
  textureBinding Texture2D $= Just tex
  textureFilter Texture2D  $= ((Nearest, Nothing), Nearest)
  rowAlignment Unpack      $= 1
  setUniform "framebuffer" (Index1 (0 :: GLint))

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

  return prog
-- }}}
-- {{{ Run
reshape :: IORef State -> IORef Bool -> Size -> IO ()
reshape state redraw size@(Size w h) = do
  viewport $= (Position 0 0, size)
  reallocSize state (fromIntegral w, fromIntegral h)
  writeIORef redraw True

texturize :: IORef State -> IO ()
texturize state = do
  State ptr _ area <- readIORef state
  let (w, h) = areaScreen area
  texImage2D
    Nothing NoProxy 0 Luminance8
    (TextureSize2D (fromIntegral w) (fromIntegral h))
    0
    (PixelData Luminance UnsignedByte ptr)

data Mode = Idle | Drag (GLfloat, GLfloat) | Zoom (GLfloat, GLfloat)

mouse :: IO (GLfloat, GLfloat)
mouse = get mousePos >>= screenToGL

screenToGL :: Position -> IO (GLfloat, GLfloat)
screenToGL (Position x y) = do
  Size w h <- get windowSize
  return (fromIntegral x / fromIntegral w, fromIntegral y / fromIntegral h)

run :: IORef State -> IO ()
run state = do
  GL.clearColor $= Color4 0 0 0 0

  prog <- initGL

  quit   <- newIORef False
  dirty  <- newIORef True
  redraw <- newIORef True
  mode   <- newIORef Idle
  pos    <- newIORef (0.0, 0.0)

  GLFW.windowSizeCallback $= reshape state redraw
  GLFW.disableSpecial GLFW.AutoPollEvent

  GLFW.windowRefreshCallback $= writeIORef dirty True

  GLFW.keyCallback $= \k s ->
    when (s == GLFW.Press) $ case k of
      GLFW.SpecialKey GLFW.ESC -> writeIORef quit True
      GLFW.CharKey 'Q'         -> writeIORef quit True
      GLFW.CharKey 'D'         -> readIORef state >>= print
      _                        -> print k

  GLFW.windowCloseCallback $= (writeIORef quit True >> return True)

  let loop = do
        GLFW.waitEvents

        whenRef redraw $ do
          update state
          texturize state
          writeIORef redraw False
          writeIORef dirty True

        whenRef dirty $ do
          clear [ ColorBuffer ]

          currentProgram $= Just prog
          drawArrays TriangleStrip 0 4
          currentProgram $= Nothing

          strokeRectangle (0::GLfloat, 0) (0.9, 0.9)

          readIORef mode >>= \m -> case m of
            Idle       -> return ()
            Drag _     -> return ()
            Zoom start -> readIORef pos >>= strokeRectangle start

          GLFW.swapBuffers
          writeIORef dirty False

        unlessRef quit loop

      idleMode = do
        writeIORef mode Idle
        writeIORef dirty True

        GLFW.mousePosCallback $= \_ -> return ()
        GLFW.mouseButtonCallback $= \b _ -> case b of
          ButtonLeft   -> dragMode
          ButtonRight  -> zoomMode
          ButtonMiddle -> mouse >>= print
          _            -> return ()

      dragMode = do
        mouse >>= writeIORef mode . Drag
        writeIORef dirty True

        GLFW.mousePosCallback $= \_ -> do
          writeIORef dirty True

        GLFW.mouseButtonCallback $= \b _ -> case b of
          ButtonLeft -> idleMode
          _          -> return ()

      zoomMode = do
        mouse >>= \p -> do
          writeIORef mode $ Zoom p
          writeIORef pos  $ p
        writeIORef dirty True

        GLFW.mousePosCallback $= \p -> do
          screenToGL p >>= writeIORef pos
          writeIORef dirty True

        GLFW.mouseButtonCallback $= \b _ -> case b of
          ButtonRight -> idleMode
          _           -> return ()

  idleMode
  loop
  where
    whenRef ref io   = readIORef ref >>= (`when` io)
    unlessRef ref io = readIORef ref >>= (`unless` io)
-- }}}

main :: IO ()
main = do
  GLFW.initialize >>= (`unless` error "Failed to initialize GLFW")
  GLFW.openWindow (GL.Size 800 600) [GLFW.DisplayRGBBits 8 8 8] GLFW.Window >>=
    (`unless` (GLFW.terminate >> error "Failed to open GLFW window"))
  GLFW.windowTitle $= "Fractlas"

  withState run

  GLFW.closeWindow
  GLFW.terminate

-- vim: set fdm=marker :
