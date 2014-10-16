module Properties
  ( prop_clampLow
  , prop_clamp
  , prop_scaleInRange
  , prop_scale0
  ) where

import Fractals.Math
import Fractals.Utility
import Test.QuickCheck

inRange :: Ord a => (a, a) -> a -> Bool
inRange (a, b) x = x >= a && x <= b

prop_clampLow :: Int -> Int -> Bool
prop_clampLow a x = clampLow a x >= a

prop_clamp :: Int -> Int -> Int -> Bool
prop_clamp i j x = inRange range (clamp range x)
  where
    range | i <= j    = (i, j)
          | otherwise = (j, i)

nonLargePositiveInt :: Int -> Gen Int
nonLargePositiveInt 0 = return 1
nonLargePositiveInt n = choose (1, n*1000)

forAllScale f =
  forAll (sized nonLargePositiveInt) $ \a ->
    forAll (sized nonLargePositiveInt) $ \b ->
      forAll (choose (0, a)) $ f a b

prop_scaleInRange :: Property
prop_scaleInRange =
  forAll (sized nonLargePositiveInt) $ \a ->
    forAll (sized nonLargePositiveInt) $ \b ->
      forAll (choose (0, a)) $ \i ->
        inRange (0, b) (scale a b i)

prop_scale0 :: Property
prop_scale0 = forAllScale $ \a b _ -> scale a b 0 == 0
