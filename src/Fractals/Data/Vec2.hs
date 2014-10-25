module Fractals.Data.Vec2 where

-- |Definition of a 2d 'Int' vector.
data Vec = Vec !Int !Int
  deriving Show

{-# INLINE (.+.) #-}
-- |Defintion of addition for 'Vec'.
--
-- Provieded as a new operator because implementing 'Num' is non-trivial.
(.+.) :: Vec -> Vec -> Vec
(.+.) (Vec x1 y1) (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)
