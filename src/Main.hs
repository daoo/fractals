module Main where

import Data.Complex
import Data.Ratio
import Fractals.Complex
import Fractals.Definitions
import Fractals.Generator

example :: [[Int]]
example = generate maxAbs iterations (julia ((-0.4):+0.6)) (centered screen plane)
  where
    maxAbs     = 4.0
    iterations = 200

    screen = (273, 78)
    plane  = (4.1, 2.1)

showASCII :: Int -> Int -> Char
showASCII m i = chars !! index
  where
    index = truncate ((i % m) * len)
    len   = fromIntegral $ length chars
    chars = " -~=e@$"

main :: IO ()
main = mapM_ putStrLn $ (map . map) (showASCII 200) example
