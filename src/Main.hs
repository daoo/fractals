module Main where

import Data.Complex
import Data.Ratio
import Fractals
import Generator

example :: [[Int]]
example = generate maxAbs iterations (mandelbrot 2) topleft delta screen
  where
    screen :: (Int, Int)

    maxAbs     = 4.0
    iterations = 200

    screen@(w, h) = (273, 78)

    plane = 4.1 :+ 2.1

    topleft = negate (realPart plane / 2.0) :+ negate (imagPart plane / 2.0)
    delta = realPart plane / realToFrac w :+ imagPart plane / realToFrac h

showASCII :: Int -> Int -> Char
showASCII m i = chars !! index
  where
    index = truncate ((i % m) * len)
    len   = fromIntegral $ length chars
    chars = " -~=e@$"

main :: IO ()
main = mapM_ putStrLn $ (map . map) (showASCII 200) example
