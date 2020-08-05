module Homotopy.Core.Expansion where

import Data.Enum (pred, succ)
import Data.Graph (Graph, empty, insertEdge, insertVertex)
import Data.List (List(..), foldl, length, tail, zip, (..), (:))
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.Tuple (Tuple(..), swap)
import Homotopy.Core.Common (Height(..))
import Homotopy.Core.Diagram (DiagramN, cospans, regularSlices, singularSlices, size, toDiagramN)
import Prelude (Unit, bind, map, unit, ($), (-), (<<<))
import Data.List.NonEmpty as NE
import Homotopy.Core.Rewrite (Cospan, Rewrite, singularImage)
import Unsafe.Coerce (unsafeCoerce)

contract :: DiagramN -> Maybe DiagramN
contract d = unsafeCoerce unit

type Edge k
  = Tuple k k

deltaGraph :: Partial => DiagramN -> Graph (Tuple Height Int) Unit
deltaGraph d =
  let
    ordinalGraph = foldl (\acc (Tuple h o) -> insertOrdinal h o acc) empty $ singularOrdinals
  in
    foldl (\acc (Tuple s t) -> insertEdge s t acc) ordinalGraph bidirectionalEdges
  where
  regularOrdinals :: List (Tuple Height Int)
  regularOrdinals =
    let
      ordinals = map (size <<< toDiagramN) $ regularSlices d
    in
      zip (map Regular (0 .. (NE.length ordinals - 1))) (NE.toList ordinals)

  singularOrdinals :: List (Tuple Height Int)
  singularOrdinals =
    let
      ordinals = map (size <<< toDiagramN) $ singularSlices d
    in
      zip (map Singular (0 .. (length ordinals - 1))) ordinals

  bidirectionalEdges :: List (Edge (Tuple Height Int))
  bidirectionalEdges = do
    Tuple (Tuple h o) (Tuple l r) <- zip (fromJust $ tail regularOrdinals) $ spans $ cospans d -- list of spans, with their ordinal and height
    do
      i <- (0 .. o)
      let
        f = singularImage l

        g = singularImage r

        e = Tuple (Tuple (fromJust $ pred h) (f i)) (Tuple (fromJust $ succ h) (g i))
      e : swap e : Nil

spans :: List Cospan -> List (Tuple Rewrite Rewrite)
spans Nil = Nil

spans (c : Nil) = Nil

spans (c : c' : cs) = Tuple c.backward c'.forward : spans (c' : cs)

insertOrdinal :: Height -> Int -> Graph (Tuple Height Int) Unit -> Graph (Tuple Height Int) Unit
insertOrdinal h o g = foldl (\acc i -> insertEdge (Tuple h (i - 1)) (Tuple h i) acc) withVertices $ fromMaybe Nil $ tail (0 .. o)
  where
  withVertices = foldl (\acc key -> insertVertex key unit acc) g $ map (\i -> Tuple h i) (0 .. o)