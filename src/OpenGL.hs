module Main where

import Control.Monad
import Data.Array.Storable
import Data.IORef
import Data.Word
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Image
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit

type Array = Image RGB StorableArray (Int, Int, Int) Word8
data State = State Array !Int Area

newStateRef :: IO (IORef State)
newStateRef = do
  img <- new (1920, 1080)
  newIORef $ State img 100 (aspectCentered (1920, 1080) 4.3 (-2.0:+0))

maxabs :: R
maxabs = 4

render :: Array -> Int -> Area -> IO ()
render img iter area = void $ fillArray
  (areaScreen area)
  (areaTopLeft area)
  (areaDelta area)
  (\n x y -> write img n $ toRgb $ greyscale iter $ mandelbrot2 (x:+y) maxabs iter)
  3

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

checkedLinkProgram :: Program -> IO ()
checkedLinkProgram prog = do
  linkProgram prog
  reportErrors
  ok <- get (linkStatus prog)
  unless ok $ do
    get (programInfoLog prog) >>= putStrLn
    deleteObjectNames [prog]
    ioError (userError "program linking failed")

fragmentShader :: String
fragmentShader =
  "#version 130\n\

  \precision highp float;\n\

  \in  vec2 texCoord;\n\
  \out vec4 fragmentColor;\n\

  \uniform sampler2D framebuffer;\n\
  \uniform int framebufferSamples;\n\

  \void main() {\n\
  \  fragmentColor = (1.0 / float(framebufferSamples)) *\n\
  \    texture2D(framebuffer, texCoord);\n\
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

  -- Framebuffer texture
  [fbTexture] <- genObjectNames 1 :: IO [TextureObject]
  textureFilter Texture2D  $= ((Linear', Nothing), Linear')
  activeTexture            $= TextureUnit 0
  textureBinding Texture2D $= Just fbTexture
  setUniform "frambuffer" (TextureUnit 0)

  -- VBO
  -- TODO

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
  reshapeCallback $= Just (reshape ref)
  keyboardMouseCallback $= Just input
  displayCallback $= (display ref)
  mainLoop

reshape :: IORef State -> Size -> IO ()
reshape ref (Size w h) = do
  State arr iter area <- readIORef ref
  let area' = area { areaScreen = (fromIntegral w, fromIntegral h) }
  render arr iter area
  writeIORef ref (State arr iter area')

texturize :: IORef State -> IO ()
texturize ref = do
  State (Image arr) _ area <- readIORef ref
  withStorableArray arr (\ptr -> tex (size $ areaScreen area) (PixelData RGB UnsignedByte ptr))

  where
    size (w, h) = TextureSize2D (fromIntegral w) (fromIntegral h)
    tex s pixeldata = texImage2D Nothing NoProxy 0 RGB8 s 0 pixeldata

display :: IORef State -> IO ()
display ref = do
  clear [ ColorBuffer ]
  State img iter area <- readIORef ref
  render img iter area
  texturize ref
  flush
