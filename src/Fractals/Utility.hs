{-# LANGUAGE BangPatterns #-}
module Fractals.Utility where

{-# INLINE square #-}
square :: Num a => a -> a
square x = x * x
