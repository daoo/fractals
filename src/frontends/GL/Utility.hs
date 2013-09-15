module GL.Utility
  ( compileAndLink
  , setUniform
  , createBuffer
  , createVAO
  , createActiveTexture
  , strokeRectangle
  ) where

import Control.Monad
import Data.Array.Storable
import Foreign.Ptr
import Foreign.Storable
import Fractals.Geometry
import Graphics.Rendering.OpenGL as GL

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

checkedLinkProgram :: Program -> IO ()
checkedLinkProgram prog = do
  linkProgram prog
  ok <- get (linkStatus prog)
  unless ok $ do
    get (programInfoLog prog) >>= putStrLn
    deleteObjectNames [prog]
    ioError (userError "program linking failed")

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

createActiveTexture :: IO TextureObject
createActiveTexture = do
  [tex] <- GL.genObjectNames 1
  GL.activeTexture               $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just tex
  GL.textureFilter GL.Texture2D  $= ((GL.Nearest, Nothing), GL.Nearest)
  GL.rowAlignment GL.Unpack      $= 1
  return tex

strokeRectangle :: Rectangle -> IO ()
strokeRectangle (Rectangle (Vec x1 y1) (Vec x2 y2)) =
  renderPrimitive LineLoop $ do
    color $ Color3 (1.0::GLfloat) 0 0
    vertex $ Vertex2 (fromIntegral x1 :: GLfloat) (fromIntegral y1 :: GLfloat)
    vertex $ Vertex2 (fromIntegral x2 :: GLfloat) (fromIntegral y1 :: GLfloat)
    vertex $ Vertex2 (fromIntegral x2 :: GLfloat) (fromIntegral y2 :: GLfloat)
    vertex $ Vertex2 (fromIntegral x1 :: GLfloat) (fromIntegral y2 :: GLfloat)
