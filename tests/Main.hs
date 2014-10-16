module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Properties

main :: IO ()
main = defaultMain properties

properties :: TestTree
properties = testGroup "Properties"
  [ QC.testProperty "clamp low range" prop_clampLow
  , QC.testProperty "clamp range"     prop_clamp
  , QC.testProperty "scale range"     prop_scaleInRange
  , QC.testProperty "scale 0"         prop_scale0
  ]
