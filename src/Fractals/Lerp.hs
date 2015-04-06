{-# LANGUAGE MagicHash #-}
module Fractals.Lerp where

import Data.Complex
import GHC.Base

-- $setup
-- >>> import Test.QuickCheck
-- >>> import System.Random
-- >>>
-- >>> let genInRange (a, b) = choose (a, b)
-- >>> let genLarger a = sized $ \n -> choose (a+1, a + max 2 (abs $ a * fromIntegral n))
-- >>> let genRange = arbitrarySizedIntegral >>= \a -> genLarger a >>= \b -> return (a, b)
-- >>> let genSteps (a, b) = genInRange (1, abs (b-a))
-- >>>
-- >>> let forAllLerp f = forAll genRange $ \r -> forAll (genSteps r) $ \s -> forAll (genInRange (0, s)) $ \x -> f r s x

-- |
-- prop> \a -> forAll (genLarger a) $ \b -> (b :: Int) > (a :: Int)
-- prop> forAll genRange $ \(a, b) -> (a :: Int) < (b :: Int)

{-# SPECIALIZE INLINE lerp :: Int -> (Int, Int) -> Int -> Int #-}
-- |Definition of linear interpolation.
--
-- prop> forAllLerp $ \(a, b) s _ -> lerp s (a, b) 0 == a
-- prop> forAllLerp $ \(a, b) s _ -> lerp s (a, b) s == b
-- prop> forAllLerp $ \(a, b) s x -> lerp s (a, b) x >= a
-- prop> forAllLerp $ \(a, b) s x -> lerp s (a, b) x <= b
lerp :: Integral a => Int    -- ^Number of steps
                   -> (a, a) -- ^Interpolation range
                   -> Int    -- ^Offset
                   -> a      -- ^Interpolated value
--lerp s (a, b) x = a + ((fromIntegral x * (b-a)) `div` fromIntegral s)
lerp s (a, b) x = (a*(fromIntegral s - fromIntegral x) + b * fromIntegral x) `div` fromIntegral s

-- |Unsafe linear interpolation, does not check for division by zero.
--
-- prop> forAllLerp $ \(a, b) s x -> unsafeLerpWord s (a, b) 0 == a
-- prop> forAllLerp $ \(a, b) s x -> unsafeLerpWord s (a, b) s == b
-- prop> forAllLerp $ \(a, b) s x -> unsafeLerpWord s (a, b) x >= a
-- prop> forAllLerp $ \(a, b) s x -> unsafeLerpWord s (a, b) x <= b
-- prop> forAllLerp $ \(a, b) s x -> fromIntegral (unsafeLerpWord s (a, b) x) == lerp (fromIntegral s) ((fromIntegral a) :: Int, fromIntegral b) (fromIntegral x)
unsafeLerpWord :: Word -> (Word, Word) -> Word -> Word
--unsafeLerpWord (W# s) ((W# a), (W# b)) (W# x) = W# (plusWord# a (quotWord# (timesWord# x (minusWord# b a)) s))
unsafeLerpWord (W# s) (W# a, W# b) (W# x) = W# (quotWord# (plusWord# (timesWord# a (minusWord# s x)) (timesWord# b x)) s)

{-# SPECIALIZE INLINE lerpFractional :: Int -> (Double, Double)                 -> Int -> Double #-}
{-# SPECIALIZE INLINE lerpFractional :: Int -> (Complex Double, Complex Double) -> Int -> Complex Double #-}
lerpFractional :: (Num a, Fractional a) => Int -> (a, a) -> Int -> a
lerpFractional s (a, b) x = a + (fromIntegral x * (b-a)) / fromIntegral s

lerpFractionals :: (Fractional a, Num a) => Int -> (a, a) -> [a]
lerpFractionals s range = go 0
  where
    go i = if i < s then f i : go (i+1) else []

    f = lerpFractional (s-1) range . fromIntegral
