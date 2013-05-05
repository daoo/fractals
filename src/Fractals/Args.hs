{-# LANGUAGE LambdaCase #-}
module Fractals.Args
  ( parseArgs
  ) where

import Control.Applicative
import Control.Monad.State
import Fractals.Complex
import Fractals.Definitions
import Fractals.Fractal

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

parseArgs :: [String] -> (Fractal, [String])
parseArgs args = (`runState` args) $ Fractal
  <$> (pop >>= \case
    "mandelbrot"  -> (mandelbrot . read) <$> pop
    "burningship" -> return burningShip
    "julia"       -> julia <$> popComp
    _             -> undefined)
  <*> (read <$> pop)
  <*> pure 4
  <*> popPoint
  <*> popPoint
  <*> popPoint
