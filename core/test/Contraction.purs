module Test.Contraction where

import Prelude

import Control.MonadZero (empty)
import Data.Maybe (fromJust)
import Homotopy.Core.Contraction (contract)
import Homotopy.Core.Diagram (Diagram(..), rewriteForward)
import Partial.Unsafe (unsafePartial)
import Test.Examples (twoBeadsOneLevel, twoBeadsTwoLevels, twoBeadsTwoLevelsToOne)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

main :: Spec Unit
main = do
  describe "generalised contraction" do
     describe "when contracting two beads on two singular levels into one level" do
        let r = unsafePartial $ fromJust $ contract twoBeadsTwoLevels 0 empty
        it "produces the correct rewrite" do
          r `shouldEqual` twoBeadsTwoLevelsToOne
        it "produces the correct contracted diagram" do
          unsafePartial $ rewriteForward r (DiagramN twoBeadsTwoLevels) `shouldEqual` (DiagramN twoBeadsOneLevel)
     -- TODO: test biased contractions
     pending "contracts with left bias correctly"
     pending "contracts with right bias correctly"
     -- TODO: write test for propagatedContract
     pending "contracts a subdiagram in a singular slice and propagates correctly"
     pending "contracts a subdiagram in a regular slice and propagates correctly"
     pending "contracts a subdiagram in the source slice and propagates correctly"
     pending "contracts a subdiagram in the target slice and propagates correctly"

