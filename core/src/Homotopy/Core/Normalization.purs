module Homotopy.Core.Normalisation (normalize) where

import Prelude
import Data.Array as Array
import Data.Foldable (all, any, elem, foldMap, foldl)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\), type (/\))
import Homotopy.Core.Diagram (Diagram(..), DiagramN)
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Interval as Interval
import Homotopy.Core.Rewrite (Cospan, Rewrite(..))
import Homotopy.Core.Rewrite as Rewrite
import Partial.Unsafe (unsafePartial)

normalize :: Diagram -> Diagram
normalize diagram = unsafePartial (normalize' diagram).diagram

type InputArrow
  = { source :: Diagram
    , rewrite :: Rewrite
    }

type Output a
  = { diagram :: Diagram
    , factors :: Map a Rewrite
    , degeneracy :: Rewrite
    }

data Index a
  = Forward Int
  | Backward Int
  | SingularSlice a Int

derive instance indexEq :: Eq a => Eq (Index a)

derive instance indexOrd :: Ord a => Ord (Index a)

normalize' :: Partial => Diagram -> Output Void
normalize' diagram = normalizeRelative mempty diagram

normalizeRelative :: forall a. Ord a => Partial => Map a InputArrow -> Diagram -> Output a
normalizeRelative inputs diagram
  | any (Rewrite.isIdentity <<< _.rewrite) inputs || Diagram.dimension diagram == 0 =
    { diagram
    , factors: map (\_ -> Rewrite.identity (Diagram.dimension diagram)) inputs
    , degeneracy: Rewrite.identity (Diagram.dimension diagram)
    }

normalizeRelative inputs (DiagramN diagram) = normalizeTopLevel (normalizeRecursive regular inputs diagram)
  where
  regular = map normalize' (List.fromFoldable (Diagram.regularSlices diagram))

normalizeRecursive :: forall a. Ord a => Partial => List (Output Void) -> Map a InputArrow -> DiagramN -> Output a
normalizeRecursive regular inputs diagram =
  { diagram: DiagramN diagram'
  , factors: mapWithIndex factor inputs
  , degeneracy: makeParallelRewrite diagram' sliceDegeneracies diagram
  }
  where
  dimension = Diagram.dimension (DiagramN diagram)

  -----------------------------------------------------------------------------
  cospanSubproblems :: List (Index a /\ InputArrow)
  cospanSubproblems = go 0 regular (Diagram.cospans diagram)
    where
    go i = case _, _ of
      r0 : r1 : rs, c : cs ->
        let
          forward =
            { rewrite: r0.degeneracy <> c.forward
            , source: r0.diagram
            }

          backward =
            { rewrite: r1.degeneracy <> c.backward
            , source: r1.diagram
            }
        in
          (Forward i /\ forward) : (Backward i /\ backward) : go (i + 1) (r1 : rs) cs
      _, _ -> Nil

  -----------------------------------------------------------------------------
  singularSubproblems :: a -> InputArrow -> List (Index a /\ InputArrow)
  singularSubproblems i input =
    mapWithIndex (\j source -> SingularSlice i j /\ { source, rewrite: Rewrite.slice input.rewrite j })
      $ Diagram.singularSlices (Diagram.toDiagramN input.source)

  subproblems :: List (Index a /\ InputArrow)
  subproblems = cospanSubproblems <> foldMapWithIndex singularSubproblems inputs

  -----------------------------------------------------------------------------
  -- Group the input arrows for the recursive step by their target height.
  subproblemGroup :: Index a -> Int
  subproblemGroup = case _ of
    Forward i -> i
    Backward i -> i
    SingularSlice i j -> Rewrite.singularImage (fromJust (Map.lookup i inputs)).rewrite j

  subproblemGroups :: Map Int (Map (Index a) InputArrow)
  subproblemGroups =
    foldl (Map.unionWith (<>)) mempty
      $ map (\(i /\ arrow) -> Map.singleton (subproblemGroup i) (Map.singleton i arrow))
      $ subproblems

  -----------------------------------------------------------------------------
  -- Solve each of the recursive normalisation problems for every target
  -- height, relative to the input arrows for that height.
  solutions :: Map Int (Output (Index a))
  solutions =
    Map.fromFoldable
      $ mapWithIndex (\i slice -> i /\ normalizeRelative (fromMaybe mempty $ Map.lookup i subproblemGroups) slice)
      $ Diagram.singularSlices diagram

  factors :: Map (Index a) Rewrite
  factors = foldMap _.factors solutions

  -----------------------------------------------------------------------------
  -- Assemble a diagram from the factorisations of the cospan maps.
  diagram' :: DiagramN
  diagram' =
    Diagram.unsafeMake (Diagram.source diagram)
      $ map
          ( \i ->
              { forward: fromJust $ Map.lookup (Forward i) factors
              , backward: fromJust $ Map.lookup (Backward i) factors
              }
          )
      $ listWithLength (Diagram.size diagram)

  -----------------------------------------------------------------------------
  -- Assemble the factorisations for the input arrows.
  factor :: a -> InputArrow -> Rewrite
  factor i input =
    Rewrite.RewriteN
      $ { dimension: Diagram.dimension (DiagramN diagram), cones: _ }
      $ List.filter (not <<< isIdentityCone)
      $ mapWithIndex
          ( \targetIndex targetCospan ->
              let
                sourceInterval = Rewrite.singularPreimage input.rewrite targetIndex

                sourceCospans =
                  map (\sourceIndex -> fromJust $ (Diagram.cospans $ Diagram.toDiagramN input.source) List.!! sourceIndex)
                    $ Interval.toUnfoldable sourceInterval

                slices =
                  map (\sourceIndex -> fromJust $ Map.lookup (SingularSlice i sourceIndex) factors)
                    $ Interval.toUnfoldable sourceInterval
              in
                { target: targetCospan
                , index: (unwrap sourceInterval).start
                , source: sourceCospans
                , slices
                }
          )
      $ Diagram.cospans diagram'

  -----------------------------------------------------------------------------
  -- Assemble the parallel degeneracy maps for every singular height.
  sliceDegeneracies :: List Rewrite
  sliceDegeneracies =
    map (\i -> maybe (Rewrite.identity dimension) _.degeneracy $ Map.lookup i solutions)
      $ listWithLength
      $ Diagram.size diagram

normalizeTopLevel :: forall a. Partial => Ord a => Output a -> Output a
normalizeTopLevel { diagram, factors, degeneracy } =
  { diagram: DiagramN diagram'
  , factors: factors'
  , degeneracy: topLevelDegeneracy <> degeneracy
  }
  where
  -- find trivial heights
  trivialHeights :: List Int
  trivialHeights =
    List.mapMaybe identity
      $ mapWithIndex
          ( \i cospan ->
              if isIdentityCospan cospan
                && all (\rewrite -> Interval.isEmpty (Rewrite.singularPreimage rewrite i)) factors then
                Just i
              else
                Nothing
          )
      $ Diagram.cospans
      $ Diagram.toDiagramN diagram

  -- build top level degeneracy map
  topLevelDegeneracy :: Rewrite
  topLevelDegeneracy = makeDegeneracy (Diagram.toDiagramN diagram) trivialHeights

  diagram' :: DiagramN
  diagram' = Diagram.toDiagramN $ Diagram.rewriteBackward topLevelDegeneracy diagram

  -- remove trivial cones from factors
  factors' :: Map a Rewrite
  factors' = map removeTrivialCone factors

  removeTrivialCone :: Rewrite -> Rewrite
  removeTrivialCone (RewriteN r) = RewriteN { dimension: r.dimension, cones: go 0 r.cones }
    where
    go offset = case _ of
      c : cs -> if (c.index + offset) `elem` trivialHeights then go (offset + 1) cs else c : go (offset + 1 - Rewrite.coneSize c) (c : cs)
      Nil -> Nil

-- make parallel degeneracy map
makeDegeneracy :: Partial => DiagramN -> List Int -> Rewrite
makeDegeneracy diagram targets = Rewrite.RewriteN { dimension: Diagram.dimension (DiagramN diagram), cones: go 0 targets }
  where
  cospans = Array.fromFoldable (Diagram.cospans diagram)

  go offset = case _ of
    Nil -> Nil
    t : ts ->
      { index: t + offset
      , source: Nil
      , target: fromJust $ cospans Array.!! t
      , slices: Nil
      }
        : go (offset - 1) ts

makeParallelRewrite :: DiagramN -> List Rewrite -> DiagramN -> Rewrite
makeParallelRewrite source slices target =
  RewriteN
    $ { dimension: Diagram.dimension (DiagramN source), cones: _ }
    $ mapWithIndex (\i (s /\ r /\ t) -> { index: i, source: s : Nil, target: t, slices: r : Nil })
    $ List.zip (Diagram.cospans source)
    $ List.zip slices
    $ Diagram.cospans target

isIdentityCospan :: Cospan -> Boolean
isIdentityCospan cospan = Rewrite.isIdentity cospan.forward && Rewrite.isIdentity cospan.backward

isIdentityCone :: Rewrite.Cone -> Boolean
isIdentityCone cone = (cone.target : Nil) == cone.source && all Rewrite.isIdentity cone.slices

listWithLength :: Int -> List Int
listWithLength length = if length <= 0 then Nil else List.range 0 (length - 1)
