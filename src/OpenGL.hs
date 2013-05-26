module Main where

-- TODO: Implement zooming in and out
-- TODO: Implement draging
-- TODO: Render with threads and different resolution

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

-- {{{ Fractal state
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

-- |Render the fractal and print the time it took
updateState :: IORef State -> IO ()
updateState state = do
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

compileAndLink :: String -> String -> IO Program
compileAndLink vert frag = do
  vs <- createShader vert
  fs <- createShader frag
  prog <- createProgram vs fs
  bindFragDataLocation prog "position" $= 0
  attribLocation prog "fragmentColor"  $= AttribLocation 0
  checkedLinkProgram prog
  return prog

setUniform :: Uniform a => Program -> String -> a -> IO ()
setUniform prog var val = do
  currentProgram $= Just prog
  location <- get (uniformLocation prog var)
  uniform location $= val

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

strokeRectangle :: Position -> Position -> IO ()
strokeRectangle (Position x1 y1) (Position x2 y2) = do
  renderPrimitive LineLoop $ do
    color $ (Color3 (1.0::GLfloat) 0 0)
    vertex $ (Vertex2 x1 y1)
    vertex $ (Vertex2 x2 y1)
    vertex $ (Vertex2 x2 y2)
    vertex $ (Vertex2 x1 y2)
-- }}}
-- {{{ GL State
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
  \uniform ivec2 size;\n\

  \void main() {\n\
  \  gl_Position = vec4((vec2(position) / size) * vec2(2, -2) + vec2(-1, 1), 0, 1);\n\
  \}"

data GL = GL
  { glFractalProg :: Program
  , glScreenProg :: Program
  }

initGL :: IO GL
initGL = do
  -- Shaders
  fractal <- compileAndLink texVertShader texFragShader
  screen  <- compileAndLink screenVertShader redFragShader

  -- Framebuffer texture
  [tex] <- genObjectNames 1
  activeTexture            $= TextureUnit 0
  textureBinding Texture2D $= Just tex
  textureFilter Texture2D  $= ((Nearest, Nothing), Nearest)
  rowAlignment Unpack      $= 1
  setUniform fractal "framebuffer" (Index1 (0 :: GLint))

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

  return $ GL fractal screen
-- }}}
-- {{{ Run
reshape :: Program -> IORef State -> IORef Bool -> Size -> IO ()
reshape prog state redraw size@(Size w h) = do
  setUniform prog "size" $ Vertex2 w h
  viewport $= (Position 0 0, size)
  resizeState state (fromIntegral w, fromIntegral h)
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

data Mode = Idle | Drag Position | Zoom Position

run :: IORef State -> IO ()
run state = do
  GL.clearColor $= Color4 0 0 0 0

  GL pfractal pscreen <- initGL

  quit   <- newIORef False
  dirty  <- newIORef True
  redraw <- newIORef True
  mode   <- newIORef Idle
  pos    <- newIORef $ Position 0 0

  GLFW.windowSizeCallback $= reshape pscreen state redraw
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
          updateState state
          texturize state
          writeIORef redraw False
          writeIORef dirty True

        whenRef dirty $ do
          clear [ ColorBuffer ]

          currentProgram $= Just pfractal
          drawArrays TriangleStrip 0 4

          currentProgram $= Just pscreen
          strokeRectangle (Position 0 0) (Position 100 100)

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

        GLFW.mousePosCallback $= \p -> writeIORef pos p

        GLFW.mouseButtonCallback $= \b _ -> case b of
          ButtonLeft   -> dragMode
          ButtonRight  -> zoomMode
          ButtonMiddle -> readIORef pos  >>= print
          _            -> return ()

      dragMode = do
        readIORef pos >>= writeIORef mode . Drag
        writeIORef dirty True

        GLFW.mousePosCallback $= \p -> do
          writeIORef pos p
          writeIORef dirty True

        GLFW.mouseButtonCallback $= \b _ -> case b of
          ButtonLeft -> idleMode
          _          -> return ()

      zoomMode = do
        readIORef pos >>= (writeIORef mode . Zoom)

        writeIORef dirty True

        GLFW.mousePosCallback $= \p -> do
          writeIORef pos p
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
