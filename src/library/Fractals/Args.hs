module Fractals.Args
  ( Fractal(..)
  , parseFractal
  ) where

import Control.Applicative
import Control.Monad.State
import Fractals.Area
import Fractals.Complex
import Fractals.Definitions

data Fractal = Fractal
  { fractalDefinition :: Definition
  , fractalIter :: Int
  , fractalMaxAbs :: R
  , fractalArea :: Area
  }

{-# INLINE pop #-}
pop :: State [String] String
pop = fmap head get >>= \s -> modify tail >> return s

{-# INLINE popPoint #-}
popPoint :: Read a => State [String] (a, a)
popPoint = do
  x <- pop
  y <- pop
  return (read x, read y)

{-# INLINE popComp #-}
popComp :: State [String] Comp
popComp = uncurry (:+) `fmap` popPoint

{-# INLINE parseArea #-}
parseArea :: State [String] Area
parseArea = fromRectangle <$> popPoint <*> popComp <*> popComp

{-# INLINE parseMandelbrot #-}
parseMandelbrot :: State [String] Definition
parseMandelbrot = mandelbrot . read <$> pop

{-# INLINE parseJulia #-}
parseJulia :: State [String] Definition
parseJulia = julia <$> popComp

{-# INLINE parseFractal #-}
parseFractal :: [String] -> (Fractal, [String])
parseFractal = parseFractal1 (pop >>= f)
  where
    f "mandelbrot"  = parseMandelbrot
    f "mandelbrot2" = return mandelbrot2
    f "mandelbrot3" = return mandelbrot3
    f "burningship" = return burningShip
    f "julia"       = parseJulia
    f _             = return mandelbrot2

{-# INLINE parseFractal1 #-}
parseFractal1 :: State [String] Definition -> [String] -> (Fractal, [String])
parseFractal1 frac args = (`runState` args) $ Fractal
  <$> frac <*> (read <$> pop) <*> pure 4 <*> parseArea
