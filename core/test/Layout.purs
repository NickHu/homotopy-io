module Test.Layout where

import Control.Applicative (pure)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.List (List(..), (:), zip, head)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import Effect.Exception (Error)
import Homotopy.Core.Common (Height(..), SliceIndex(..))
import Homotopy.Core.Diagram (Diagram, toDiagramN)
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Rewrite as Rewrite
import Homotopy.Core.Projection as Projection
import Homotopy.Core.Layout (solveLayout, layoutPosition)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, join, map, ($), (<<<))
import Test.Examples as Examples
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

points :: Partial => Diagram -> List { x :: SliceIndex, y :: SliceIndex }
points diagram = do
  Tuple height slice <- slicesWithHeights (toDiagramN diagram)
  map { y: height, x: _ } (heights slice)
  where
  heights = map Interior <<< enumFromTo (Regular 0) <<< Regular <<< Diagram.size

  slicesWithHeights d = zip (heights d) $ map toDiagramN $ NEL.toList $ Diagram.slices d

shouldLayoutAs :: forall m. MonadThrow Error m => Diagram -> Array (Array Number) -> m Unit
shouldLayoutAs diagram positions =
  unsafePartial
    $ let
        layout = fromJust $ solveLayout diagram
      in
        map (\{ x, y } -> layoutPosition layout y x) (points diagram) `shouldEqual` map Just (Array.toUnfoldable $ join $ positions)

main :: Spec Unit
main = do
  it "left associated multiplication" do
    (Diagram.source Examples.associativity)
      `shouldLayoutAs`
        [ [ 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ]
        , [ 0.0, 2.0, 4.0, 5.0, 6.0 ]
        , [ 0.0, 2.0, 4.0, 5.0, 6.0 ]
        , [ 0.0, 3.5, 6.0 ]
        , [ 0.0, 3.5, 6.0 ]
        ]
  it "right associated multiplication" do
    (Diagram.target Examples.associativity)
      `shouldLayoutAs`
        [ [ 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ]
        , [ 0.0, 1.0, 2.0, 4.0, 6.0 ]
        , [ 0.0, 1.0, 2.0, 4.0, 6.0 ]
        , [ 0.0, 2.5, 6.0 ]
        , [ 0.0, 2.5, 6.0 ]
        ]
