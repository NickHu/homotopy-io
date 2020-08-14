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
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Homotopy.Core.Diagram (Diagram(..), DiagramN)
import Homotopy.Core.Diagram as Diagram
import Homotopy.Core.Interval as Interval
import Homotopy.Core.Normalization.Degeneracy (Degeneracy(..))
import Homotopy.Core.Normalization.Degeneracy as Degeneracy
import Homotopy.Core.Rewrite (Cospan, Rewrite(..), makeRewriteN)
import Homotopy.Core.Rewrite as Rewrite
import Partial.Unsafe (unsafePartial)

normalize :: Diagram -> Diagram
normalize diagram = unsafePartial (normalize' diagram).diagram

type InputArrow
  = { source :: Diagram
    , degeneracy :: Degeneracy
    , rewrite :: Rewrite
    }

type Output a
  = { diagram :: Diagram
    , factors :: Map a Rewrite
    , degeneracy :: Degeneracy
    }

type Recursive a
  = { diagram :: Diagram
    , factors :: Map a Rewrite
    , degeneracy :: List Degeneracy
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
    , degeneracy: Degeneracy.identity
    }

normalizeRelative inputs (DiagramN diagram) = normalizeTopLevel (normalizeRecursive regular inputs diagram)
  where
  regular = map normalize' (List.fromFoldable (Diagram.regularSlices diagram))

normalizeRecursive :: forall a. Ord a => Partial => List (Output Void) -> Map a InputArrow -> DiagramN -> Recursive a
normalizeRecursive regular inputs diagram =
  { diagram: DiagramN diagram'
  , factors: mapWithIndex factor inputs
  , degeneracy: sliceDegeneracies
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
            { rewrite: c.forward
            , degeneracy: r0.degeneracy
            , source: r0.diagram
            }

          backward =
            { rewrite: c.backward
            , degeneracy: r1.degeneracy
            , source: r1.diagram
            }
        in
          (Forward i /\ forward) : (Backward i /\ backward) : go (i + 1) (r1 : rs) cs
      _, _ -> Nil

  -----------------------------------------------------------------------------
  singularSubproblems :: a -> InputArrow -> List (Index a /\ InputArrow)
  singularSubproblems i input =
    mapWithIndex
      ( \j source ->
          SingularSlice i j
            /\ { source
              , degeneracy: Degeneracy.slice input.degeneracy j
              , rewrite: Rewrite.slice input.rewrite (Degeneracy.singularImage input.degeneracy j)
              }
      )
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
    makeRewriteN (Diagram.dimension (DiagramN diagram))
      $ List.filter (not <<< isIdentityCone)
      $ mapWithIndex
          ( \targetIndex targetCospan ->
              let
                sourceInterval =
                  Interval.image (Degeneracy.singularPreimage input.degeneracy)
                    $ Rewrite.singularPreimage input.rewrite targetIndex

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
  sliceDegeneracies :: List Degeneracy
  sliceDegeneracies =
    map (\i -> _.degeneracy $ fromJust $ Map.lookup i solutions)
      $ listWithLength
      $ Diagram.size diagram

normalizeTopLevel :: forall a. Partial => Ord a => Recursive a -> Output a
normalizeTopLevel { diagram, factors, degeneracy } =
  { diagram: Diagram.rewriteBackward topLevelDegeneracy diagram
  , factors: map removeTrivialCones factors
  , degeneracy: Degeneracy trivialHeights (Map.fromFoldable $ mapWithIndex Tuple degeneracy)
  }
  where
  isTrivial :: Int -> Cospan -> Boolean
  isTrivial i cospan =
    isIdentityCospan cospan
      && all (\rewrite -> Interval.isEmpty (Rewrite.singularPreimage rewrite i)) factors

  -- find trivial heights
  trivialHeights :: List Int
  trivialHeights =
    List.mapMaybe identity
      $ mapWithIndex (\i cospan -> if isTrivial i cospan then Just i else Nothing)
      $ Diagram.cospans
      $ Diagram.toDiagramN diagram

  -- build top level degeneracy map
  topLevelDegeneracy :: Rewrite
  topLevelDegeneracy = makeDegeneracyRewrite (Diagram.toDiagramN diagram) trivialHeights

  removeTrivialCones :: Rewrite -> Rewrite
  removeTrivialCones (RewriteN r) = makeRewriteN r.dimension (go 0 r.cones)
    where
    go offset = case _ of
      c : cs -> if (c.index + offset) `elem` trivialHeights then go (offset + 1) cs else c : go (offset + 1 - Rewrite.coneSize c) (c : cs)
      Nil -> Nil

-- make parallel degeneracy map
makeDegeneracyRewrite :: Partial => DiagramN -> List Int -> Rewrite
makeDegeneracyRewrite diagram targets = makeRewriteN dimension (go 0 targets)
  where
  dimension = Diagram.dimension (DiagramN diagram)

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

isIdentityCospan :: Cospan -> Boolean
isIdentityCospan cospan = Rewrite.isIdentity cospan.forward && Rewrite.isIdentity cospan.backward

isIdentityCone :: Rewrite.Cone -> Boolean
isIdentityCone cone = (cone.target : Nil) == cone.source && all Rewrite.isIdentity cone.slices

listWithLength :: Int -> List Int
listWithLength length = if length <= 0 then Nil else List.range 0 (length - 1)
