module Main where

import Fractals.Args
import Fractals.Output
import System.Environment

main :: IO ()
main = getArgs >>= (putStr . call string . fst . parseFractal)
