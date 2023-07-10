module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

sanity :: TestSuite
sanity =
  suite "sanity" do
    test "it compiled"
        $ Assert.assert "the world has gone mad" true

main :: Effect Unit
main = runTest do
  sanity
