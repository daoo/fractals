module Main where

import Fractals.Args
import Fractals.Render
import System.Environment

main :: IO ()
main = getArgs >>= (putStr . call string . fst . parseFractal)
