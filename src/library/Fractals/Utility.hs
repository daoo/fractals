{-# LANGUAGE MagicHash #-}
module Fractals.Utility where

import GHC.Exts
import System.CPUTime

clampLow :: Ord a => a -> a -> a
clampLow a x | x < a     = a
             | otherwise = x

clamp :: Ord a => (a, a) -> a -> a
clamp (a, b) x | x < a     = a
               | x > b     = b
               | otherwise = x

{-# INLINE xy #-}
xy :: (a -> b) -> (t1 -> t2 -> a) -> t1 -> t2 -> b
xy f g a b = f (g a b)

-- |Square a number
{-# INLINE square #-}
square :: Num a => a -> a
square x = x * x

{-# INLINE scale #-}
-- |Scale a number from one range to another.
-- Both ranges are inclusive, start at 0 and ends at the given number. This
-- function is tuned for speed and does not do any division by zero or range
-- checks.
scale :: Int -- ^ End of the first range [0, a], must be greater than zero
      -> Int -- ^ End of the second range [0, b], must be greater than zero
      -> Int -- ^ The number within range [0, a]
      -> Int -- ^ Number in range [0, b]
scale a b i = (i * b) `unsafeQuot` a
  where
    unsafeQuot (I# x) (I# y) = I# (quotInt# x y)

{-# INLINE measureTime #-}
measureTime :: IO () -> IO ()
measureTime f = do
  start <- getCPUTime
  f
  end <- getCPUTime
  let diff = (end - start) `quot` 1000000000
  putStrLn $ "Rendered in " ++ show diff ++ " ms"
