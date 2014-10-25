{-# LANGUAGE Safe, MagicHash #-}
module Fractals.Utility where

import System.CPUTime

clampLow :: Ord a => a -> a -> a
clampLow a x
  | x < a     = a
  | otherwise = x

{-# INLINE measureTime #-}
measureTime :: IO () -> IO ()
measureTime f = do
  start <- getCPUTime
  f
  end <- getCPUTime
  let diff = (end - start) `quot` 1000000000
  putStrLn $ showString "Rendered in " $ shows diff " ms"
