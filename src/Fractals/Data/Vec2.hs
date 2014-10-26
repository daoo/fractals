module Fractals.Data.Vec2 where

data Vec2 a = !a :* !a
  deriving Show

infixl 0 :*
infixl 7 ./
infixl 6 .+.

{-# INLINE (.+.) #-}
(.+.) :: Num a => Vec2 a -> Vec2 a -> Vec2 a
(x1 :* y1) .+. (x2 :* y2) = x1 + x2 :* y1 + y2

(.-.) :: Num a => Vec2 a -> Vec2 a -> Vec2 a
(x1 :* y1) .-. (x2 :* y2) = x1 - x2 :* y1 - y2

(.*.) :: Num a => Vec2 a -> Vec2 a -> Vec2 a
(x1 :* y1) .*. (x2 :* y2) = x1 * x2 :* y1 * y2

(./.) :: (Num a, Fractional a) => Vec2 a -> Vec2 a -> Vec2 a
(x1 :* y1) ./. (x2 :* y2) = x1 / x2 :* y1 / y2

{-# INLINE (./) #-}
(./) :: (Num a, Fractional a) => Vec2 a -> a -> Vec2 a
(x :* y) ./ a = x / a :* y / a

{-# INLINE vfromIntegral #-}
vfromIntegral :: (Integral a, Num b) => Vec2 a -> Vec2 b
vfromIntegral (x :* y) = fromIntegral x :* fromIntegral y
