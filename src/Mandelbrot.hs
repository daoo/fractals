module Main where

import Codec.Image.DevIL
import Data.Array.Unsafe
import Fractals.Area
import Fractals.Complex
import Fractals.Definitions
import Fractals.Render

main :: IO ()
main = ilInit >> rgbaArray mandelbrot2' 200 4 area >>= unsafeFreeze >>= writeImage "dist/mandelbrot.png"
  where
    screen@(w, h) = (1920, 1080)
    aspect = realToFrac w / realToFrac h

    area = fromRectangle
      screen
      (4.3      :+ 4.3 / aspect)
      (-4.3/2.0 :+ 4.3 / aspect / 2.0)
