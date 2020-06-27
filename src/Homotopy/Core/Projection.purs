module Homotopy.Core.Projection (pointDepth, generatorAt, wireDepths) where

import Data.List (List(..), head, (!!), (:))
import Data.Maybe (Maybe(..), fromJust)
import Homotopy.Core.Common (Generator, SliceIndex(..), Boundary(..), Height(..))
import Homotopy.Core.Diagram (Diagram)
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Rewrite (Rewrite)
import Homotopy.Core.Rewrite as Rewrite
import Prelude (bind, map, min, pure, ($), (+), (>=))

pointDepth :: Partial => Diagram -> Int -> Maybe Int
pointDepth diagram height = minMaybe forward backward
  where
  cospan = fromJust (Diagram.cospans diagram !! height)

  forward = head (Rewrite.targets cospan.forward)

  backward = head (Rewrite.targets cospan.backward)

  minMaybe Nothing Nothing = Nothing

  minMaybe (Just x) Nothing = Just x

  minMaybe Nothing (Just y) = Just y

  minMaybe (Just x) (Just y) = Just (min x y)

generatorAt :: Partial => Diagram -> List SliceIndex -> Maybe Generator
generatorAt diagram Nil = Just (Diagram.toGenerator diagram)

generatorAt diagram (Interior (Singular height) : Nil)
  | Diagram.dimension diagram >= 2 = do
    slice <- Diagram.sliceAt diagram (Interior (Singular height))
    generatorAt slice (height' : Nil)
    where
    height' = case pointDepth diagram height of
      Nothing -> Boundary Source
      Just depth -> Interior (Singular depth)

generatorAt diagram (p : ps) = do
  slice <- Diagram.sliceAt diagram p
  generatorAt slice ps

wireDepths ::
  Partial =>
  Diagram ->
  Int ->
  Int ->
  Maybe { source :: List (Maybe Int), target :: List (Maybe Int) }
wireDepths diagram row col = do
  cospan <- Diagram.cospans diagram !! row
  sourceSlice <- Diagram.sliceAt diagram $ Interior (Regular row)
  targetSlice <- Diagram.sliceAt diagram $ Interior (Regular (row + 1))
  let
    source = rewriteWireDepths cospan.forward sourceSlice col

    target = rewriteWireDepths cospan.backward targetSlice col
  pure { source, target }

rewriteWireDepths :: Partial => Rewrite -> Diagram -> Int -> List (Maybe Int)
rewriteWireDepths rewrite source targetHeight = do
  sourceHeight <- Rewrite.singularPreimage rewrite targetHeight
  pure $ map (Rewrite.singularImage (Rewrite.slice rewrite sourceHeight)) $ pointDepth source sourceHeight
