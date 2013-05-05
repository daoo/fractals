{-# LANGUAGE LambdaCase #-}
module Fractals.Args where

import Control.Applicative
import Control.Monad.State
import Fractals.Complex
import Fractals.Definitions
import Fractals.Fractal

pop :: State [String] String
pop = fmap head get >>= \s -> modify tail >> return s

popPoint :: Read a => State [String] (a, a)
popPoint = do
  x <- pop
  y <- pop
  return (read x, read y)

popComp :: State [String] Comp
popComp = uncurry (:+) `fmap` popPoint

parseArgs :: [String] -> Fractal
parseArgs args = (`evalState` args) $ Fractal
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
