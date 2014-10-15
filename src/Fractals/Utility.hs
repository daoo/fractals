{-# LANGUAGE MagicHash #-}
module Fractals.Utility where

import Control.Monad
import Data.IORef
import System.CPUTime

clampLow :: Ord a => a -> a -> a
clampLow a x
  | x < a     = a
  | otherwise = x

clamp :: Ord a => (a, a) -> a -> a
clamp (a, b) x
  | x < a     = a
  | x > b     = b
  | otherwise = x

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) f g a b = f (g a b)

{-# INLINE measureTime #-}
measureTime :: IO () -> IO ()
measureTime f = do
  start <- getCPUTime
  f
  end <- getCPUTime
  let diff = (end - start) `quot` 1000000000
  putStrLn $ showString "Rendered in " $ shows diff " ms"

whenRef, unlessRef :: IORef Bool -> IO () -> IO ()
whenRef ref io   = readIORef ref >>= (`when` io)
unlessRef ref io = readIORef ref >>= (`unless` io)
