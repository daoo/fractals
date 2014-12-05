{-# LANGUAGE Safe, MagicHash #-}
module Fractals.Utility where

clampLow :: Ord a => a -> a -> a
clampLow a x
  | x < a     = a
  | otherwise = x
