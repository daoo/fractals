module Main where

import Control.Monad
import Fractals.Utility
import Test.Framework
import Tests.Coloring
import Tests.Utility
import qualified Test.Framework.Providers.QuickCheck2 as QC
import qualified Test.Framework.Providers.SmallCheck as SC

main = defaultMain
  [ testGroup "Fractals.Utility"
    [ SC.testProperty "propScaleInRange" propScaleInRange
    , SC.testProperty "propScale0" propScale0
    , SC.testProperty "propScaleB" propScaleB
    ]
  , testGroup "Fractals.Coloring"
    [ QC.testProperty "propRgbaRgbInv" propRgbaRgbInv
    ]
  ]
