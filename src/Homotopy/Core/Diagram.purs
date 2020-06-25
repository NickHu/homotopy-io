module Homotopy.Core.Diagram
  ( Diagram
  , identity
  , dimension
  , size
  , cospans
  , toGenerator
  , fromGenerator
  , source
  , target
  , slices
  , sliceAt
  , singularSlices
  , regularSlices
  , internalizeHeight
  ) where

import Data.Foldable (length, maximum)
import Data.List (List(..), concatMap, drop, last, scanl, tail, take, (!!), (:))
import Data.Maybe (Maybe(..), fromJust)
import Homotopy.Core.Generator (Generator)
import Homotopy.Core.Rewrite (Cone, Cospan, Height(..), Rewrite(..), SliceIndex(..), coneSize)
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, map, otherwise, ($), (&&), (*), (+), (-), (<), (<>), (==), (>), (>=))

-- | A diagram is either 0-dimensional, in which case it consists of a
-- | generator, or n-dimensional (for n > 0), in which case it has a source
-- | $n-1$ diagram a list of (n - 1) cospans
data Diagram
  = Diagram0 Generator
  | DiagramN { source :: Diagram, cospans :: List Cospan }

-- | Promotes an n-dimensional diagram to an (n + 1)-dimensional diagram with
-- | the original diagram as its unique regular slice.
identity :: Diagram -> Diagram
identity d = DiagramN { source: d, cospans: Nil }

-- | Every diagram has a dimension
dimension :: Diagram -> Int
dimension (Diagram0 _) = 0

dimension (DiagramN { source: s }) = 1 + dimension s

-- | Every non-zero-dimensional diagram has a list of cospans.
cospans :: Partial => Diagram -> List Cospan
cospans (DiagramN { cospans: cs }) = cs

-- | The size of a diagram in the top dimension. For (n + 1)-dimensional
-- | diagrams this is the number of cospans or equivalently the number of
-- | singular slices. The size of a 0-dimensional diagram is always 1.
size :: Diagram -> Int
size (Diagram0 _) = 1

size (DiagramN { cospans: cs }) = length cs

instance eqDiagram :: Eq Diagram where
  eq (Diagram0 g) (Diagram0 g') = g == g'
  eq (DiagramN d) (DiagramN d') = d.source == d'.source && d.cospans == d.cospans
  eq _ _ = false

-- | The maximum dimensional generator of a diagram.
toGenerator :: Diagram -> Generator
toGenerator (Diagram0 g) = g

toGenerator d = unsafePartial $ fromJust $ maximum $ map toGenerator $ slices d

-- | Creates a new diagram for a generator.
-- |
-- | When n-dimensional source and target diagrams are given, an
-- | (n + 1)-dimensional diagram is created in which the specified generator
-- | transforms the source into the target diagram.  When no boundary is given,
-- | a 0-dimensional diagram is created instead.
fromGenerator :: Maybe { source :: Diagram, target :: Diagram } -> Generator -> Diagram
fromGenerator Nothing generator = Diagram0 generator

fromGenerator (Just { source: s, target: t }) generator = DiagramN { source: s, cospans: cospan : Nil }
  where
  cospan = { forward: rewriteCone generator s, backward: rewriteCone generator t }

  rewriteCone :: Generator -> Diagram -> Rewrite
  rewriteCone g (Diagram0 base) = Rewrite0 { source: base, target: g }

  rewriteCone g base@(DiagramN { cospans: cs }) =
    RewriteN
      { dimension: dimension base
      , cones:
          ( { index: 0
            , source: cs
            , target: { forward: rewriteCone g (unsafePartial (source base)), backward: rewriteCone g (unsafePartial (target base)) }
            , slices: map (\sl -> rewriteCone g sl) $ unsafePartial $ singularSlices base
            }
              : Nil
          )
      }

-- | The source slice of an (n + 1)-dimensional diagram.
source :: Partial => Diagram -> Diagram
source (DiagramN { source: s }) = s

-- | The target slice of an (n + 1)-dimensional diagram.
target :: Partial => Diagram -> Diagram
target d = fromJust $ last $ slices d

-- | The list of all the slices of an (n + 1)-dimensional diagram.
slices :: Partial => Diagram -> List Diagram
slices d = scanl (\s r -> r s) (source d) $ concatMap genRewrites $ cospans d
  where
  genRewrites :: Partial => Cospan -> List (Diagram -> Diagram)
  genRewrites { forward: fw, backward: bw } = rewriteForward fw : rewriteBackward bw : Nil

  rewriteForward :: Partial => Rewrite -> Diagram -> Diagram
  rewriteForward (Rewrite0 rewrite) (Diagram0 _) = Diagram0 rewrite.target

  rewriteForward RewriteI (Diagram0 g) = Diagram0 g

  rewriteForward (RewriteN { cones }) (DiagramN d') = DiagramN { source: d'.source, cospans: go d'.cospans 0 cones }
    where
    go :: List Cospan -> Int -> List Cone -> List Cospan
    go cspans _ Nil = cspans

    go cspans i (c : cs) = go (take (c.index + i) cspans <> c.target : drop (c.index + i + coneSize c) cspans) (i - coneSize c + 1) cs

  rewriteBackward :: Rewrite -> Diagram -> Diagram
  rewriteBackward (Rewrite0 rewrite) (Diagram0 _) = Diagram0 rewrite.source

  rewriteBackward RewriteI (Diagram0 g) = Diagram0 g

  rewriteBackward (RewriteN { cones }) (DiagramN d') = DiagramN { source: d'.source, cospans: go d'.cospans 0 cones }
    where
    go :: List Cospan -> Int -> List Cone -> List Cospan
    go cspans _ Nil = cspans

    go cspans i (c : cs) = go (take (c.index + i) cspans <> c.source <> drop (c.index + i + 1) cspans) (i + coneSize c - 1) cs

-- | The slice of an (n + 1)-dimensional diagram at a particular height.
-- |
-- | Fails when the diagram is 0-dimensional or the index is out of bounds.
sliceAt :: Partial => Diagram -> SliceIndex -> Diagram
sliceAt d i = fromJust $ slices d !! heightToIndex (internalHeight)
  where
  internalHeight = fromJust (internalizeHeight d i)

  heightToIndex (Singular h) = h * 2 + 1

  heightToIndex (Regular h) = h * 2

-- | The list of the singular slices of an (n + 1)-dimensional diagram.
singularSlices :: Partial => Diagram -> List Diagram
singularSlices d = everyOther (fromJust $ tail (slices d))

-- | The list of the regular slices of an (n + 1)-dimensional diagram.
regularSlices :: Partial => Diagram -> List Diagram
regularSlices d = everyOther (slices d)

everyOther :: forall a. List a -> List a
everyOther Nil = Nil

everyOther (x : Nil) = x : Nil

everyOther (x : _ : xs) = x : everyOther xs

internalizeHeight :: Diagram -> SliceIndex -> Maybe Height
internalizeHeight _ Source = Just (Regular 0)

internalizeHeight d Target = Just (Regular (size d))

internalizeHeight d (Height (Regular h))
  | h < 0 = Nothing
  | h > size d = Nothing
  | otherwise = Just (Regular h)

internalizeHeight d (Height (Singular h))
  | h < 0 = Nothing
  | h >= size d = Nothing
  | otherwise = Just (Singular h)
