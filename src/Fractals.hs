module Fractals where

import Complex

type FractalFunc = Comp -> Comp -> Comp

{-# INLINE mandelbrot #-}
mandelbrot :: Int -> FractalFunc
mandelbrot 0 c _ = c
mandelbrot 1 c z = z + c
mandelbrot 2 c z = z * z + c
mandelbrot a c z = z ^ a + c

{-# INLINE burningShip #-}
burningShip :: FractalFunc
burningShip (r :+ i) c = (abs r :+ abs i) ** 2 + c
