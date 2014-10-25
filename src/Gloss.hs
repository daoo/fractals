{-# LANGUAGE BangPatterns, LambdaCase #-}
module Main (main) where

import Data.Complex
import Data.Monoid
import Data.Word
import Foreign.ForeignPtr
import Fractals.Coloring.Palette
import Fractals.Data.Area
import Fractals.Data.Rectangle
import Fractals.Data.Size
import Fractals.Data.Vec2
import Fractals.Definitions
import Fractals.Storage
import Fractals.Utility
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit

colorMap :: ColorMap PackedRGBA
colorMap = mkColorMap colors1

resizeAreaFromRect :: Area -> Rectangle -> Area
resizeAreaFromRect area (Rectangle a b) = resizePlane a' s area
  where
    a'@(ar :+ ac) = transform a
    (br :+ bc)    = transform b
    s             = abs (br - ar) :+ abs (bc - ac)

    transform = screenToPlane area

data Mode = Idle | Zoom Vec Vec
  deriving Show

data UI = UI
  { uiIter   :: !Int
  , uiMaxAbs :: !Double
  , uiArea   :: !Area
  , uiDirty  :: !Bool
  , uiPtr    :: !(ForeignPtr Word32)

  , uiMode      :: !Mode
  , uiAreaStack :: [Area]
  } deriving Show

defaultUI :: ForeignPtr Word32 -> UI
defaultUI ptr = UI
  { uiIter   = 100
  , uiMaxAbs = 4
  , uiArea   = area
  , uiPtr    = ptr
  , uiDirty  = False

  , uiMode      = Idle
  , uiAreaStack = []
  }

  where
    defsize = mkSize 800 600
    area    = fromAspectCentered defsize 4.3 (0:+0)

mapMode :: (Mode -> Mode) -> UI -> UI
mapMode f ui = ui { uiMode = f (uiMode ui) }

-- | Resize and reallocate the storage to accustom the new size, does not
-- render the fractal.
resizeUI :: Size -> UI -> IO UI
resizeUI size ui = do
  ptr <- newForeignPtr32 size
  return $ ui
    { uiPtr   = ptr
    , uiArea  = resizeScreen size (uiArea ui)
    , uiDirty = True

    , uiMode      = Idle
    , uiAreaStack = []
    }

pushArea :: (Area -> Area) -> UI -> UI
pushArea f ui = ui
  { uiArea      = f (uiArea ui)
  , uiAreaStack = uiArea ui : uiAreaStack ui
  , uiDirty     = True
  }

mapIter :: (Int -> Int) -> UI -> UI
mapIter f ui = ui
  { uiIter  = clampLow 1 $ f (uiIter ui)
  , uiDirty = True
  }

redrawUI :: UI -> IO UI
redrawUI ui = if uiDirty ui
  then do
    fillForeignPtr32
      (uiPtr ui)
      (unsafeColorRgba colorMap (uiIter ui))
      mandelbrot2
      (uiIter ui)
      (uiMaxAbs ui)
      (uiArea ui)

    return (ui { uiDirty = False })

  else return ui

main :: IO ()
main = do
  ptr <- newForeignPtr32 (mkSize 800 600)
  playIO
    (InWindow "Fractals" (800, 600) (0, 0))
    black
    30
    (defaultUI ptr)
    render
    input
    (const redrawUI)

render :: UI -> IO Picture
render !ui = return (fractal `mappend` mode)
  where
    w = width  $ areaScreen $ uiArea ui
    h = height $ areaScreen $ uiArea ui

    fractal = bitmapOfForeignPtr w h (castForeignPtr (uiPtr ui)) False

    mode = case uiMode ui of
      Idle           -> mempty
      Zoom start end -> color red $ rectangle start end

    rectangle (Vec x1 y1) (Vec x2 y2) = lineLoop [(x1', y1'), (x2', y1'), (x2', y2'), (x1', y2')]
      where
        x1' = fromIntegral x1
        y1' = fromIntegral y1
        x2' = fromIntegral x2
        y2' = fromIntegral y2

input :: Event -> UI -> IO UI
input = \case
  EventKey (MouseButton mb) ms _ mp -> \ui -> return $ case (mb, ms, uiMode ui) of

    (LeftButton  , Down , Idle)           -> doRecenter (roundpos mp) ui
    (RightButton , Down , Idle)           -> doZoomStart (roundpos mp) ui
    (RightButton , Up   , Zoom start end) -> doZoomEnd start end ui
    (_           , Down , Zoom _ _)       -> mapMode (const Idle) ui

    _ -> ui

  EventKey (SpecialKey KeyEsc) Up _ _ -> \_  -> exitSuccess
  EventKey (Char 'q')          Up _ _ -> \_ -> exitSuccess

  EventKey (SpecialKey KeyF1) Up _ _ -> \ui -> print ui >> return ui

  EventKey (Char '+') Up _ _ -> return . mapIter (+ 10)
  EventKey (Char '-') Up _ _ -> return . mapIter (subtract 10)

  EventMotion mp     -> return . mapMode (zoomUpdate mp)
  EventResize (w, h) -> resizeUI (mkSize (fromIntegral w) (fromIntegral h))

  _ -> return

  where
    roundpos (x, y) = Vec (round x) (round y)

    centerArea pos area = setPlaneCenter (screenToPlane area pos) area

    zoomArea start end area = resizeAreaFromRect area $
      fixAspect (areaScreen area) start end

    zoomUpdate _  Idle           = Idle
    zoomUpdate mp (Zoom start _) = Zoom start (roundpos mp)

    doRecenter = pushArea . centerArea
    doZoomStart = mapMode . (\p _ -> Zoom p p)

    doZoomEnd start end = mapMode (const Idle) . pushArea (zoomArea start end)
