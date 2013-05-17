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
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit

-- {{{ State
data State = State
  { stateImg :: Ptr Word8
  , stateIter :: !Int
  , stateArea :: Area
  }

newStateRef :: IO (IORef State)
newStateRef = do
  ptr <- newRgbaPtr (1920, 1080)
  newIORef $ State ptr 100 (aspectCentered (1920, 1080) 4.3 (0:+0))

maxabs :: R
maxabs = 4

resize :: IORef State -> (Int, Int) -> IO ()
resize ref size = do
  State ptr iter area <- readIORef ref
  free ptr
  ptr' <- newRgbaPtr size
  writeIORef ref $ State ptr' iter (resizeScreen size area)

render :: IORef State -> IO ()
render ref = do
  State ptr iter area <- readIORef ref
  measureTime $ fillRgbaPtr greyscale mandelbrot2 iter maxabs area ptr
-- }}}
-- {{{ OpenGL helpers
createShader :: Shader s => String -> IO s
createShader src = do
  [shader] <- genObjectNames 1
  shaderSource shader $= [src]
  compileShader shader
  reportErrors
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
  reportErrors
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
        reportErrors
        uniform location $= val
  currentProgram $= Just prog
  reportErrors

  -- Framebuffer texture
  [tex] <- genObjectNames 1
  activeTexture            $= TextureUnit 0
  textureBinding Texture2D $= Just tex
  textureFilter Texture2D  $= ((Nearest, Nothing), Nearest)
  setUniform "framebuffer" (Index1 (0 :: GLint))
  reportErrors

  -- VAO
  let quad :: [GLfloat]
      quad =
        [  1.0, -1.0, 0.0
        ,  1.0,  1.0, 0.0
        , -1.0, -1.0, 0.0
        , -1.0,  1.0, 0.0
        ]

  vao <- createBuffer quad >>= createVAO
  reportErrors
  bindVertexArrayObject $= Just vao

input :: KeyboardMouseCallback
input key state _ _ =
  case (key, state) of
    (Char '\27', Down) -> exitSuccess
    (Char 'q', Down)   -> exitSuccess
    _                  -> return ()

main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [ SingleBuffered, RGBMode ]
  _ <- createWindow progname

  initGL
  ref <- newStateRef

  reshapeCallback       $= Just (reshape ref)
  keyboardMouseCallback $= Just input
  displayCallback       $= display ref

  mainLoop

  State ptr _ _ <- readIORef ref
  free ptr

reshape :: IORef State -> Size -> IO ()
reshape ref size@(Size w h) = do
  viewport $= (Position 0 0, size)
  resize ref (fromIntegral w, fromIntegral h)

texturize :: IORef State -> IO ()
texturize ref = do
  State ptr _ area <- readIORef ref
  let (w, h) = areaScreen area
  texImage2D
    Nothing NoProxy 0 RGBA8
    (TextureSize2D (fromIntegral w) (fromIntegral h))
    0
    (PixelData RGBA UnsignedByte ptr)

display :: IORef State -> IO ()
display ref = do
  clear [ ColorBuffer ]
  render ref
  texturize ref
  drawArrays TriangleStrip 0 4
  flush

-- vim: set fdm=marker :
