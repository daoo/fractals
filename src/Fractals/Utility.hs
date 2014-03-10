{-# LANGUAGE MagicHash, BangPatterns #-}
module Fractals.Utility where

import Control.Monad
import Data.IORef
import GHC.Base
import System.CPUTime

clampLow :: Ord a => a -> a -> a
clampLow a x | x < a     = a
             | otherwise = x

clamp :: Ord a => (a, a) -> a -> a
clamp (a, b) x | x < a     = a
               | x > b     = b
               | otherwise = x

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) f g a b = f (g a b)

-- |Square a number
square :: Num a => a -> a
square x = x * x

-- |Scale a number from one range to another.
-- Both ranges are inclusive, start at 0 and ends at the given number. This
-- function is tuned for speed and does not do any division by zero or range
-- checks.
scale :: Int -- ^ End of the first range [0, a], must be greater than zero
      -> Int -- ^ End of the second range [0, b], must be greater than zero
      -> Int -- ^ The number within range [0, a]
      -> Int -- ^ Number in range [0, b]
scale a b i = (i * b) `quotInt` a

{-# INLINE measureTime #-}
measureTime :: IO () -> IO ()
measureTime !f = do
  start <- getCPUTime
  f
  end <- getCPUTime
  let diff = (end - start) `quot` 1000000000
  putStrLn $ showString "Rendered in " $ shows diff " ms"

whenRef, unlessRef :: IORef Bool -> IO () -> IO ()
whenRef ref io   = readIORef ref >>= (`when` io)
unlessRef ref io = readIORef ref >>= (`unless` io)
