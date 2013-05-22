module Tests.Coloring where

import Fractals.Coloring
import Test.SmallCheck
import Test.SmallCheck.Series

propRgbaRgbInv :: RGB -> Bool
propRgbaRgbInv rgb = toRgb (toRgba rgb) == rgb
