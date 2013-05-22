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
  ptr <- newGreyscalePtr (800, 600)
  let iter = 100
      area = aspectCentered (800, 600) 4.3 (0:+0)
  state <- newIORef $ State ptr iter area
  f state
  State ptr' _ _ <- readIORef state
  free ptr'

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
-- }}}
-- {{{ Shaders
fragmentShader :: String
fragmentShader =
  "#version 130\n\

  \precision highp float;\n\

  \in  vec2 texCoord;\n\
  \out vec4 fragmentColor;\n\

  \uniform sampler2D framebuffer;\n\

  \void main() {\n\
  \  fragmentColor = texture2D(framebuffer, texCoord);\n\
  \}"

vertexShader :: String
vertexShader =
  "#version 130\n\

  \in  vec3 position;\n\
  \out vec2 texCoord;\n\

  \void main() {\n\
  \  texCoord    = (position.xy + vec2(1.0)) * 0.5;\n\
  \  gl_Position = vec4(position, 1);\n\
  \}"
-- }}}
-- {{{ Init GL
initGL :: IO ()
initGL = do
  -- Shaders
  vs <- createShader vertexShader
  fs <- createShader fragmentShader
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

run :: IORef State -> IO ()
run state = do
  GL.clearColor $= Color4 0 0 0 0

  initGL

  quit   <- newIORef False
  dirty  <- newIORef True
  redraw <- newIORef True

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
          drawArrays TriangleStrip 0 4
          GLFW.swapBuffers
          writeIORef dirty False

        unlessRef quit loop

      waitForPress = do
        GLFW.mousePosCallback    $= \_ -> return ()

        GLFW.mouseButtonCallback $= \_ _ -> do
          waitForRelease

      waitForRelease = do
        GLFW.mousePosCallback $= \_ -> return ()

        GLFW.mouseButtonCallback $= \_ _ -> do
          waitForPress

  waitForPress
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
