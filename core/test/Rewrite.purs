module Test.Rewrite where

import Prelude

import Test.Spec (Spec, describe, pending)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)

main :: Spec Unit
main = do
  describe "rewrite composition" do
     pending "succeeds in composing composable rewrites"
     pending "fails in composing uncomposable rewrites"

