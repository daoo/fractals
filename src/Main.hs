module Main where

import Data.Complex
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

{-# INLINE showASCII #-}
showASCII :: Int -> Int -> Char
showASCII m i = chars !! ((i * length chars) `div` m)
  where
    chars = " -~=e@$"

main :: IO ()
main = mapM_ (putStrLn . map (showASCII 200)) example
