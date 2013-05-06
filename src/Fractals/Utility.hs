{-# LANGUAGE BangPatterns #-}
module Fractals.Utility where

{-# INLINE (.:) #-}
(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) f g a b = f (g a b)

{-# INLINE square #-}
square :: Num a => a -> a
square x = x * x
