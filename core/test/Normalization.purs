module Test.Normalization where

import Data.Array ((..))
import Data.List (List(..), (:))
import Homotopy.Core.Normalization.Degeneracy (Degeneracy(..))
import Homotopy.Core.Normalization.Degeneracy as Degeneracy
import Homotopy.Core.Interval (Interval (..))
import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

main :: Spec Unit
main = do
  describe "Degeneracy" do
    let 
      d = Degeneracy (2 : 4 : 5 : Nil) mempty

    it "singularImage" do
      let
        actual = map (Degeneracy.singularImage d) (0 .. 4)

        expected = [0, 1, 3, 6, 7]

      actual `shouldEqual` expected
     
    it "singularPreimage" do
      let
        actual = map (Degeneracy.singularPreimage d) (0 .. 7)

        expected = [
          Interval { start: 0, length: 1 },
          Interval { start: 1, length: 1 },
          Interval { start: 2, length: 0 },
          Interval { start: 2, length: 1 },
          Interval { start: 3, length: 0 },
          Interval { start: 3, length: 0 },
          Interval { start: 3, length: 1 },
          Interval { start: 4, length: 1 }
        ]

      actual `shouldEqual` expected
