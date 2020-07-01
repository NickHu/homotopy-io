module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Diagram as TestDiagram

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Diagram" (TestDiagram.main)
