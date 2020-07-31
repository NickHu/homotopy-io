module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Contraction as TestContraction
import Test.Diagram as TestDiagram
import Test.Layout as TestLayout
import Test.Projection as TestProjection
import Test.Rewrite as TestRewrite
import Test.Normalization as TestNormalization

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Contraction" TestContraction.main
        describe "Diagram" TestDiagram.main
        describe "Layout" TestLayout.main
        describe "Normalization" TestNormalization.main
        describe "Projection" TestProjection.main
        describe "Rewrite" TestRewrite.main
