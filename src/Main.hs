module Main where

import Control.Applicative
import Control.Monad.State
import Fractals.Complex
import Fractals.Definitions
import Fractals.Fractal
import Fractals.Output
import System.Environment

pop :: State [String] String
pop = fmap head get >>= \s -> modify tail >> return s

popPoint :: Read a => State [String] (a, a)
popPoint = do
  x <- pop
  y <- pop
  return (read x, read y)

popComp :: State [String] Comp
popComp = uncurry (:+) `fmap` popPoint

parseArgs :: State [String] Fractal
parseArgs = do
  str <- pop

  fractal <- case str of
    "mandelbrot"  -> (mandelbrot . read) <$> pop
    "burningship" -> return burningShip
    "julia"       -> julia <$> popComp
    _             -> undefined

  iters   <- read <$> pop
  screen  <- popPoint
  topleft <- popPoint
  plane   <- popPoint
  return $ Fractal fractal iters topleft plane screen

main :: IO ()
main = do
  fractal <- evalState parseArgs <$> getArgs
  putStrLn $ showFractal (fractalIter fractal) $ render fractal
