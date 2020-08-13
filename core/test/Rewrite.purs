module Test.Rewrite where

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array as Array
import Data.Foldable (for_)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error, error)
import Homotopy.Core.Common (Boundary(..), Generator(..), Height(..), SliceIndex(..))
import Homotopy.Core.Diagram (Diagram(..), attach)
import Homotopy.Core.Diagram as Diagram
import Prelude (Unit, bind, discard, map, mod, pure, ($), (-), (/), (==), unit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Homotopy.Core.MiniMake
import Unsafe.Coerce
import Homotopy.Core.Rewrite

main :: Spec Unit
main = do
  it "testing getComponentTargets" do
    let
      -- leaving irrelevant parts of the structure undefined
      cSource = (unsafeCoerce unit : unsafeCoerce unit : Nil)
      c2Source = (unsafeCoerce unit : Nil)
      c = { index: 1, source : cSource, target : unsafeCoerce unit, slices: unsafeCoerce unit }
      c' = { index: 4, source : c2Source, target: unsafeCoerce unit, slices: unsafeCoerce unit }
      coneList = c' : c : Nil
      targetList = 3 : 1 : Nil
    shouldEqual (listConeTargets coneList) targetList
  it "testing removeCone" do
    let
      cSource = (unsafeCoerce unit : unsafeCoerce unit : Nil)
      c2Source = (unsafeCoerce unit : Nil)
      c = { index: 1, source : cSource, target : unsafeCoerce unit, slices: unsafeCoerce unit }
      c' = { index: 4, source : c2Source, target: unsafeCoerce unit, slices: unsafeCoerce unit }
      c'' = { index: 6, source : c2Source, target: unsafeCoerce unit, slices: unsafeCoerce unit }
    shouldEqual (removeCone (c'' : c' : c : Nil) 2) (c'' : c' : c : Nil)
    shouldEqual (removeCone (c'' : c' : c : Nil) 3) (c'' : c : Nil)
