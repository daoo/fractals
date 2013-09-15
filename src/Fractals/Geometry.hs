{-# LANGUAGE BangPatterns #-}
module Fractals.Geometry
  ( Vec(Vec)
  , Size
  , Point
  , Rectangle(..)
  , fixAspect
  ) where

data Vec = Vec !Int !Int
  deriving Show

type Size  = Vec
type Point = Vec

(.+) :: Vec -> Vec -> Vec
(.+) (Vec x1 y1) (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)

-- |Represents a rectangle from two corner points.
--
-- The first point is always top left and the second point is always bottom
-- right.
data Rectangle = Rectangle
  { rectA :: !Point
  , rectB :: !Point
  } deriving Show

area :: Size -> Int
area (Vec w h) = abs $ w * h

fromPoints :: Point -> Point -> Rectangle
fromPoints (Vec x1 y1) (Vec x2 y2) = Rectangle
  (Vec (min x1 x2) (min y1 y2))
  (Vec (max x1 x2) (max y1 y2))

findLargest :: Size -> Point -> Point -> Size
findLargest (Vec w h) (Vec x1 y1) (Vec x2 y2)
  | area byw < area byh = byh
  | otherwise           = byw
  where
    rw = x2 - x1
    rh = y2 - y1

    byw = Vec rw (rw*h `quot` w)
    byh = Vec (rh*w `quot` h) rh

-- |Resize a rectangle to the largest rectangle that goes through the bottom
-- left point with the specified aspect ratio.
fixAspect :: Size -> Point -> Point -> Rectangle
fixAspect aspect a b = fromPoints a (a .+ (findLargest aspect a b))
