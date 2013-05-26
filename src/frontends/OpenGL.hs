module Main where

-- TODO: Implement zooming in and out
-- TODO: Implement draging
-- TODO: Render with threads and different resolution

import Control.Monad
import Data.IORef
import Fractals.Area
import GL.Fractal
import GL.Util
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

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
          ButtonMiddle -> do
            p@(Position x y) <- readIORef pos
            State _ _ area   <- readIORef state
            print p
            print $ screenToPlane area (fromIntegral x, fromIntegral y)

          _ -> return ()

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
        start@(Position sx sy) <- readIORef pos
        writeIORef mode $ Zoom start

        writeIORef dirty True

        GLFW.mousePosCallback $= \p -> do
          writeIORef pos p
          writeIORef dirty True

        GLFW.mouseButtonCallback $= \b _ -> case b of
          ButtonRight -> do
            Position ex ey <- readIORef pos
            modAreaState state $ \area ->
              let conv        = (\(x, y) -> screenToPlane area (fromIntegral x, fromIntegral y))
                  topleft     = conv (sx, sy)
                  bottomright = conv (ex, ey)
                  size        = bottomright - topleft
               in resizePlane topleft size area
            writeIORef redraw True
            idleMode

          _ -> return ()

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
