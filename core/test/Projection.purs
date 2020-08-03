module Test.Projection where

import Prelude
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Homotopy.Core.Common (SliceIndex(..), Height(..), Generator(..))
import Homotopy.Core.Diagram (Diagram(..))
import Homotopy.Core.Projection as Projection
import Partial.Unsafe (unsafePartial)
import Test.Examples as Examples
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

main :: Spec Unit
main = do
  it "associativtity diagram projects correctly to 2d" do
    let
      diagram = DiagramN Examples.associativity

      project y x = unsafePartial $ Projection.generatorAt diagram (Interior y : Interior x : Nil)

      generator2 = Generator { id: 2, dimension: 2 }
    project (Regular 0) (Singular 0) `shouldEqual` Just generator2
    project (Regular 0) (Singular 1) `shouldEqual` Just generator2
    project (Regular 1) (Singular 0) `shouldEqual` Just generator2
    project (Regular 1) (Singular 1) `shouldEqual` Just generator2
