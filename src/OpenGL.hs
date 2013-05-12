module Main where

import Control.Monad
import Data.Array.IO
import Data.IORef
import Data.Word
import Fractals.Area
import Fractals.Coloring
import Fractals.Complex
import Fractals.Definitions
import Fractals.Render
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit

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

  [fbTexture] <- genObjectNames 1 :: IO [TextureObject]
  textureFilter Texture2D  $= ((Linear', Nothing), Linear')
  activeTexture            $= TextureUnit 0
  textureBinding Texture2D $= Just fbTexture
  setUniform "frambuffer" (TextureUnit 0)

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
  ref <- newIORef undefined
  reshapeCallback $= Just (reshape ref)
  keyboardMouseCallback $= Just input
  displayCallback $= (display ref)
  mainLoop

type Array = IOUArray (Int, Int, Int) Word8
data State = State Array !Int Area

maxabs :: R
maxabs = 4

render :: Array -> Int -> Area -> IO ()
render arr iter area = void $ fillRgbaArray
  (areaScreen area)
  (areaTopLeft area)
  (areaDelta area)
  (\x y -> greyscale iter $ mandelbrot2' (x:+y) maxabs iter)
  arr

reshape :: IORef State -> Size -> IO ()
reshape ref (Size w h) = do
  State arr iter area <- readIORef ref
  let area' = area { areaScreen = (fromIntegral w, fromIntegral h) }
  render arr iter area
  writeIORef ref (State arr iter area')

texturize :: IORef State -> IO ()
texturize ref = do
  State arr iter area <- readIORef ref
  let (w, h) = areaScreen area
  --texImage2D Nothing 0 RGBA8 (TextureSize2D w h) 0 (PixelData<
  return ()

display :: IORef State -> IO ()
display ref = do
  clear [ ColorBuffer ]
  flush
