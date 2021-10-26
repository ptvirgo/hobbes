module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Halogen.Svg.Attributes.Transform (Transform(..))

import Hobbes

testScale :: TestSuite
testScale =
  suite "fitSizeToScale" do
    test "fitSizeToScale handles increases" do
      Assert.equal
        ( Scale 2.0 2.0)
        ( fitSizeToScale (Size { height : 200, width : 100 }) (Size { height : 50, width : 50 } ))
      Assert.equal
        ( Scale 4.0 4.0)
        ( fitSizeToScale (Size { height : 100, width : 200 }) (Size { height : 25, width : 25 } ))
    test "fitSizeToScale handles descreases" do
      Assert.equal
        ( Scale 0.5 0.5 )
        ( fitSizeToScale (Size { height : 50, width : 75 }) (Size { height : 100, width : 100 } ))
      Assert.equal
        ( Scale 0.5 0.5 )
        ( fitSizeToScale (Size { height : 100, width : 150 }) (Size { height : 200, width : 200 }))


main :: Effect Unit
main = runTest do
  testScale

