module Fractals.Complex
  ( module Data.Complex
  , magSquared
  , Comp
  ) where

import Data.Complex

type Comp = Complex Double

{-# INLINE magSquared #-}
magSquared :: Num a => Complex a -> a
magSquared (r :+ i) = r * r + i * i
